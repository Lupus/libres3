(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2014 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License version 2 as     *)
(*  published by the Free Software Foundation.                            *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA 02110-1301 USA.                                                    *)
(*                                                                        *)
(*  Special exception for linking this software with OpenSSL:             *)
(*                                                                        *)
(*  In addition, as a special exception, Skylable Ltd. gives permission   *)
(*  to link the code of this program with the OpenSSL library and         *)
(*  distribute linked combinations including the two. You must obey the   *)
(*  GNU General Public License in all respects for all of the code used   *)
(*  other than OpenSSL. You may extend this exception to your version     *)
(*  of the program, but you are not obligated to do so. If you do not     *)
(*  wish to do so, delete this exception statement from your version.     *)
(**************************************************************************)

open CodedIO
open Unix
open Configfile
module StringMap = Map.Make(String)

let server_name = ("libres3-" ^ Version.version)
type substr = string * int * int

type headers = {
  status: Nethttp.http_status;
  reply_headers: CanonRequest.header list;
  content_type: string option;
  content_length: int64 option;
  last_modified: float option;
  etag: string option;
}

module type Server = sig
  type t
  type u
  type 'a monad
  val log: t -> string -> unit
  val send_headers: t -> headers -> u monad
  val send_data: u -> string * int * int -> unit monad
end

module type Sig = sig
  type 'a monad
  type source
  type server
  type 'a request = {
    server: server;
    body_file: string option;
    info: CanonRequest.request_info;
    meth: 'a;
  } constraint 'a = [> `DELETE | `GET | `HEAD | `POST of source | `PUT of source]

  type t
  val init : unit -> t monad
  val handle_request: t -> 'a request -> unit monad
end
module Make
  (U: Sigs.SXIOSig)
  (IO: Sigs.EventIOSig with type 'a t = 'a U.M.t)
  (S: Server with type 'a monad = 'a U.M.t)
: (Sig with type source = U.source
       and type 'a monad = 'a U.M.t
       and type server = S.t
  ) = struct
  type 'a monad = 'a U.M.t
  type source = U.source
  type server = S.t
  type ('a) request = {
    server: server;
    body_file: string option;
    info: CanonRequest.request_info;
    meth: 'a;
  } constraint 'a = [> `DELETE | `GET | `HEAD | `POST of source | `PUT of source]
  open IO.Op

  let debug_output =
    try
      let name = Printf.sprintf "/tmp/libres3-debug.%d.log" (Unix.getpid ()) in 
      if (Sys.getenv "LIBRES3_DEBUG") = "1" then begin
        Some (open_out_gen [Open_wronly;Open_append;Open_creat] 0o600 name)
      end else None
    with Not_found -> None;;

  let add_std_headers ~id ~id2 ?dbg_body headers =
    let result =
      List.rev_append [
        "Server", server_name;
        "x-amz-id-2", id2;
        "x-amz-request-id", RequestId.to_string id
      ] headers in
    begin match debug_output with
    | None -> ()
    | Some ch ->
        output_string ch "<<<<Reply headers\n";
        List.iter (fun (n,v) -> Printf.fprintf ch "\t%s:%s\n" n v) headers;
        begin match dbg_body with
        | None -> ()
        | Some str ->
          Printf.fprintf ch "<<<<Reply body (%d) bytes\n%s\n------\n\n"
            (String.length str) str;
        end;
        flush ch
    end;
    result
  ;;

  let return_string ~id ~id2 ~req ~status ?last_modified ~reply_headers ~content_type str =
    let headers = add_std_headers ~id ~id2 reply_headers ~dbg_body:str in
    S.send_headers req.server {
      status = status;
      reply_headers = headers;
      last_modified = last_modified;
      content_type = Some content_type;
      content_length = Some (Int64.of_int (String.length str));
      etag = None;
    } >>= fun sender ->
    if req.meth = `HEAD then return () (* ensure HEAD has empty body, but all
    headers preserved, including Content-Length *)
    else S.send_data sender (str, 0, String.length str)
  ;;

  let return_empty ~req ~canon ~status ~reply_headers =
    let headers =
      add_std_headers
        ~id:canon.CanonRequest.id
        ~id2:(CanonRequest.gen_debug ~canon)
        reply_headers in
    S.send_headers req.server {
      status = status;
      reply_headers = headers;
      last_modified = None;
      content_type = None;
      content_length = Some 0L;
      etag = None
    } >>= fun _ -> return ()
  ;;

  let invalid_range length =
    let header = Headers.make_content_range (`Bytes (None, Some length))
    in
    IO.fail ( Error.ErrorReply ( Error.InvalidRange, [], header));;

  let parse_ranges headers content_length  =
    let default_last = Int64.sub content_length 1L in
    match Headers.get_range headers with
    | Some (`Bytes [range]) ->
      begin match range with
      | None, Some suffix_length ->
        let first = Int64.sub content_length suffix_length in
        if first < 0L then
          Some (0L, default_last)
        else
          Some (first, default_last)
      | Some prefix, None ->
        Some (prefix, default_last)
      | Some first, Some last ->
        if first > last then
          None (* syntactically invalid *)
        else if last > default_last then
          Some (first, default_last)
        else
          Some (first, last)
      | None, None ->
        None (* syntatictically invalid *)
      end
    | None | Some (`Bytes _) ->
        (* we only support one range *)
       None
  ;;

  let send_source url ~canon ~first sender =
    if canon.CanonRequest.req_method = `HEAD then return () (* ensure HEAD's body is empty *)
    else
      U.copy url ~srcpos:first (U.of_sink (fun _ ->
        return (S.send_data sender)))
  ;;

  let return_source ~req ~canon ~content_type ~etag url =
    let headers = add_std_headers ~id:canon.CanonRequest.id
        ~id2:(CanonRequest.gen_debug ~canon) ["ETag",etag] in
    U.with_url_source url (fun source ->
      return (source.U.meta.U.size, source.U.meta.U.mtime)) >>= fun (size, mtime) ->
    match (parse_ranges canon.CanonRequest.headers size) with
    | None ->
        S.send_headers req.server {
          status = `Ok;
          reply_headers = ("Accept-Ranges","bytes") :: headers;
          last_modified = Some mtime;
          content_type = Some content_type;
          content_length = Some size;
          etag = None;
        } >>= send_source url ~canon ~first:0L
    | Some (first, last) as range ->
        if first > last then
          invalid_range size (* not satisfiable *)
        else
          let h = Headers.make_content_range
            (`Bytes (range, Some size)) in
          let length = Int64.add 1L (Int64.sub last first) in
          S.send_headers req.server {
            status = `Partial_content;
            reply_headers = List.rev_append h headers;
            last_modified = Some mtime;
            content_type = Some content_type;
            content_length = Some length;
            etag = None;
          } >>= send_source url ~canon ~first
  ;;

  let return_xml ?log ~id ~id2 ~req ~status ~reply_headers xml =
    let str = CodedIO.Xml.to_string xml in
    begin match log with
    | None -> ()
    | Some l ->
        l (
          Printf.sprintf "Replying with code %d: %s"
            (Nethttp.int_of_http_status status) str
          )
    end;
    return_string ~id ~id2 ~req ~status ~reply_headers
      ~content_type:"application/xml" str;;

  let return_xml_canon ?log ~req ~canon ~status ~reply_headers xml =
    return_xml ?log ~id:canon.CanonRequest.id ~id2:(CanonRequest.gen_debug ~canon)
      ~req ~status ~reply_headers xml;;

  let str_of_method = function
    | `DELETE -> "DELETE"
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `POST _ -> "POST"
    | `PUT _ -> "PUT"
    | _ -> "N/A"

  let return_error_xml ~id ~id2 ~req ~path ~headers code detail =
    let code_str, code_msg, status = Error.info code in
    let rid = RequestId.to_string id in
    let code_msg =
      let rec findmsg = function
        | ("SXErrorMessage", sxmsg) :: _ -> code_msg ^ " (" ^ sxmsg ^ ")"
        | ("LibreS3ErrorMessage", msg) :: _ -> msg
        | _ :: tl -> findmsg tl
        | [] -> code_msg in
      findmsg detail in
    let xml =
      if code = Error.AccessDenied then
        Xml.tag "Error" [
          Xml.tag "Message" [Xml.d code_msg];
          Xml.tag "RequestId" [Xml.d rid];
          Xml.tag "Code" [Xml.d code_str]
        ]
      else
        let error_tags = [
          Xml.tag "Message" [Xml.d code_msg];
          Xml.tag "Resource" [Xml.d path];(* TODO: should this just be
          the path without query args? *)
          Xml.tag "RequestId" [Xml.d rid];
          Xml.tag "Code" [Xml.d code_str];
          Xml.tag "Method" [Xml.d (str_of_method req.meth)]
        ]
        and detail_tags = List.map (fun (tag,contents) ->
          Xml.tag tag [Xml.d contents]
        ) detail in
        Xml.tag "Error" (List.rev_append error_tags detail_tags)
    in
    let log = if code = Error.NoSuchKey then None
      else Some (S.log req.server) in
    return_xml ?log ~id ~id2 ~req ~status ~reply_headers:headers xml;;

  let return_error code details =
    IO.fail (Error.ErrorReply (code, details, []));;

  let map_method = function
    | (`DELETE | `GET | `HEAD | `POST _| `PUT _) as a -> a
    | _ -> `UNSUPPORTED

  let max_input_xml = 65536

  let read_all ~input ~max =
    let buf = Buffer.create Configfile.small_buffer_size in
    U.copy (`Source input) ~srcpos:0L (U.of_sink (fun _ ->
      return (fun (str,pos,len) ->
        if (Buffer.length buf) + len > max then
          fail (Failure ("input too large"))
        else begin
          Buffer.add_substring buf str pos len;
          return ()
        end
      ))
    ) >>= fun () ->
    return (Buffer.contents buf);;

  let parse_input_xml_opt body root_tag validate f =
    read_all ~input:body ~max:max_input_xml >>= function
    | "" -> f (validate [])
    | str ->
      try match Xml.parse_string str with
      | `El (((_, tag),_), children) when tag = root_tag ->
        f (validate children)
      | `El (((_, tag),_), _) ->
        return_error Error.MalformedXML [
          "BadRootTag", tag;
          "ExpectedRootTag", root_tag
        ]
      | `Data _ ->
          assert false
      with Xmlm.Error ((line,col), err) ->
        return_error Error.MalformedXML [
          "ErrorLine", (string_of_int line);
          "ErrorColumn", (string_of_int col);
          "ErrorMessage", (Xmlm.error_message err)
        ]
  ;;

  let us_bucketname_re =
    Netstring_str.regexp "^[A-Za-z0-9._-]+$"

  let dns_bucketname_re = Netstring_str.regexp
    "^[a-z0-9][a-z0-9-]*[a-z0-9]\\([.][a-z0-9][a-z0-9-]*[a-z0-9]\\)*$"

  let regex_matches re str =
    match Netstring_str.string_match re str 0 with
    | None -> false
    | Some _ -> true;;

  let validate_bucketname bucket region =
    if bucket = "tmp" then
      Error.BucketAlreadyExists, ["BucketNameReserved", bucket]
    else
    let n = String.length bucket in
    match region with
    | None ->
      Error.MalformedXML, ["BadCreateBucketConfiguration",""]
    | Some "us-standard" ->
        (* TODO: UTF-8 characters or bytes? *)
        let n = String.length bucket in
        if n > 255 then
          Error.InvalidBucketName, ["BucketNameTooLong", string_of_int n]
        else if not (regex_matches us_bucketname_re bucket) then
          Error.InvalidBucketName, ["DoesNotMatchRegexp", bucket]
        else
          Error.NoError, []
    | Some _ ->
        if n < 3 then
          Error.InvalidBucketName, ["BucketNameTooShort", string_of_int n]
        else if not (regex_matches dns_bucketname_re bucket) then
          Error.InvalidBucketName, ["DoesNotMatchRegexp", bucket]
        else
          Error.NoError, []
  ;;

  let get_path bucket path =
    let err, _ = validate_bucketname bucket (Some "us-standard") in
    (* TODO return error by raise *)
    if err <> Error.NoError then
      invalid_arg "bucket name";
    Filename.concat !Configfile.buckets_dir (
      Filename.concat bucket (Util.sanitize_path path)
    );;
  
  (* TODO: instead the IO interface that we depend on shouldn't use URLs
   * but do the string list to URL conversion internally! *)
  let url_of_volpath_user ~user bucket path =
    let to_url bucket path =
      match !Configfile.sx_host with
      | Some host ->
          if bucket <> "" then begin
            let err, details = validate_bucketname bucket (Some "us-standard") in
            if err <> Error.NoError then
              raise (Error.ErrorReply(err, details, []));
          end;
          let path =
            if bucket = "" then [""]
            else match Neturl.split_path path with
            | [] | [""] -> ["";bucket;""]
            | "" :: rest -> "" :: bucket :: rest
            | rest -> "" :: bucket :: rest in
          let url = 
          Neturl.make_url ~encoded:false
            ~scheme:"sx"
            ~host ~path
            ~user
            SXC.syntax in
          url
      | None ->
          let p =
            if bucket = "" then Filename.concat !Configfile.buckets_dir ""
            else get_path bucket path in
          Neturl.file_url_of_local_path p in
    U.of_neturl (to_url bucket ""),
    U.of_neturl (to_url bucket path);;

  let url_of_volpath ~canon =
    url_of_volpath_user ~user:canon.CanonRequest.user;;

  let make_bucket ~req ~canon bucket =
    IO.try_catch (fun () ->
      let base, _ = url_of_volpath ~canon bucket "" in
      U.create base >>= fun () ->
      return_empty ~req ~canon ~status:`Ok ~reply_headers:["Location","/"^bucket]
    ) (function
      | Unix_error(EEXIST,_,_) ->
          return_empty ~req ~canon ~status:`Ok ~reply_headers:["Location","/"^bucket]
      | err ->
          IO.fail err
    ) ();;

  let create_bucket ~request ~canon body bucket =
    (* TODO: x-amz-grant-* for permissions *)
    parse_input_xml_opt body "CreateBucketConfiguration" (function
      | [] -> Some "us-standard"
      | [`El (((_,"LocationConstraint"),_), children)] ->
          begin match children with
          | [] | [`Data ""] -> Some "us-classic"
          | [`Data s] -> Some s
          | _ -> None
          end
      | _ ->
          None
      ) (fun region ->
        match validate_bucketname bucket region with
        | Error.NoError, [] ->
            make_bucket ~req:request ~canon bucket
        | error, detail ->
            return_error error detail
      );;

  let head_bucket ~req ~canon bucket =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
      | Some _ ->
        return_empty ~req ~canon ~status:`Ok ~reply_headers:[]
      | None ->
         return_error Error.NoSuchBucket ["Bucket", bucket]
    ;;

  let fake_owner =
      [
          (* TODO: use real uid once SX supports it *)
          Xml.tag "ID" [Xml.d Configfile.owner_id];
          Xml.tag "DisplayName" [ Xml.d Configfile.owner_name]
      ]

  module StringSet = Set.Make(String)
  let list_bucket_files2 l common =
    let contents = StringMap.fold (fun name (size, mtime, md5) accum ->
      (Xml.tag "Contents" [
        Xml.tag "Key" [Xml.d name];
        Xml.tag "LastModified" [Xml.d (
          Util.format_date mtime)
        ];
        Xml.tag "ETag" [Xml.d ("\"" ^ md5 ^ "\"")];
        Xml.tag "Size" [Xml.d (Int64.to_string size)];
        Xml.tag "StorageClass" [Xml.d "STANDARD"];
        Xml.tag "Owner" fake_owner
      ]) :: accum
    ) l [] in
    StringSet.fold (fun name accum ->
      (Xml.tag "CommonPrefixes" [
        Xml.tag "Prefix" [Xml.d (name ^ "/")]
      ]) :: accum
    ) common contents;;

  (* returns tmpfile, digest *)
  let source_of_request ~canon body =
    if Headers.has_header canon.CanonRequest.headers "x-amz-copy-source" then
    begin
      let source = (
        Headers.field_single_value canon.CanonRequest.headers
        "x-amz-copy-source" "") in
      let source_bucket, source_path =
        Util.url_split_first_component (Neturl.split_path source) in
      let decoded_path = Netencoding.Url.decode source_path in
      let _, url = url_of_volpath ~canon source_bucket decoded_path in
      U.with_url_source url (fun source ->
        return source.U.meta.U.mtime) >>= fun mtime ->
      return (url, mtime)
    end else begin
      let mtime = Unix.gettimeofday () in
      return (`Source body, mtime)
      (* TODO: have to calc md5 for tmpfile
      match request.body_file with
      | None -> return (`Source body, mtime)
      | Some tmp -> return (`Tmp tmp, mtime)*)
    end;;

  let md5_stream digestref md5 stream () =
    stream () >>= fun (str,pos,len) ->
    if len = 0 then
      digestref := Cryptokit.transform_string (Cryptokit.Hexa.encode ())
        md5#result
    else begin
      md5#add_substring str pos len;
    end;
    return (str, pos, len);;

  let md5_seek_source digestref source pos =
    source.U.seek pos >>= fun stream ->
    let md5 = Cryptokit.Hash.md5 () in
    return (md5_stream digestref md5 stream);;

  let md5_source digestref source =
    `Source {
      U.meta = source.U.meta;
      seek = md5_seek_source digestref source
    };;

  let md5_stream2 md5 stream () =
    stream () >>= fun (str,pos,len) ->
    if len > 0 then
      md5#add_substring str pos len;
    return (str, pos, len);;

  let md5_seek_source2 md5 source pos =
    source.U.seek pos >>= fun stream ->
    return (md5_stream2 md5 stream);;

  let md5_source2 md5 source =
    `Source {
      U.meta = source.U.meta;
      seek = md5_seek_source2 md5 source
    };;

  let copy_tourl ~canon body url =
    source_of_request ~canon body >>= fun (src,mtime) ->
    let source = match src with
    | `Tmp _ -> `Source body
    | (`Source _ | `Url _) as s -> s in
    U.copy source ~srcpos:0L url >>= fun () ->
    let digest = ref "" in
    U.with_url_source url (fun source ->
        U.copy (md5_source digest source) ~srcpos:0L `Null
    ) >>= fun () ->
    (* TODO: check for .., check Content-MD5, store it,
     * and store Content-Type*)
    return (!digest, mtime);;

  let copy_object ~canon ~request body bucket path =
    (* TODO: check that body is empty *)
    let _, url = url_of_volpath ~canon bucket path in
    (* TODO: handle the other x-amz-copy* and x-amz-meta* directives too *)
    IO.try_catch
      (fun () ->
        copy_tourl ~canon body url
      )
      (function
      | Unix.Unix_error(Unix.EISDIR,_,_) ->
          (* COPY of directory is not supported by S3 *)
          return_error Error.NoSuchKey []
      | Unix.Unix_error(Unix.ENOENT,_,_) ->
          return_error Error.NoSuchKey ["Key",path]
      | e -> IO.fail e
      ) () >>= fun (digest, lastmodified) ->
    return_xml_canon ~req:request ~canon ~status:`Ok ~reply_headers:[] (
      Xml.tag "CopyObjectResult" [
        Xml.tag "LastModified" [Xml.d (Util.format_date lastmodified)];
        Xml.tag "ETag" [Xml.d ("\"" ^ digest ^ "\"")]
      ]
    );;

  let meta_key = "libres3-etag-md5"

  let with_source ~canon body f =
    source_of_request ~canon body >>= function
      | `Source s, _ -> f s
      | `Url _ as url, _ ->
          U.with_url_source url f

  let md5_metafn md5 digestref () =
    digestref := Cryptokit.transform_string (Cryptokit.Hexa.encode ())
      md5#result;
    [meta_key, !digestref]

  let put_object ~canon ~request body bucket path =
    with_source ~canon body (fun src ->
    let md5 = Cryptokit.Hash.md5 () in
    let source = md5_source2 md5 src in
    IO.try_catch
      (fun () ->
        let _, url = url_of_volpath ~canon bucket path in
        let digestref = ref "" in
        U.copy ~metafn:(md5_metafn md5 digestref) source ~srcpos:0L url >>= fun () ->
        return_empty ~canon ~req:request ~status:`Ok
          ~reply_headers:["ETag","\"" ^ !digestref ^ "\""](*TODO: store etag *)
      )
      (function
      | Unix.Unix_error(Unix.ENOENT,_,bucket) ->
          return_error Error.NoSuchBucket ["Bucket",bucket]
      | e ->
          IO.fail e
      ) ()
    );;

  let send_default_acl ~req ~canon =
    return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] (
      Xml.tag "AccessControlPolicy" [
        Xml.tag "Owner" [
          Xml.tag "ID" [Xml.d Configfile.owner_id];
          Xml.tag "DisplayName" [Xml.d Configfile.owner_name]
        ];
        Xml.tag "AccessControlList" [
          Xml.tag "Grant" [
            Xml.tag ~attrs:[
              Xml.attr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance";
              Xml.attr "xsi:type" "CanonicalUser"
            ] "Grantee" [
              Xml.tag "ID" [Xml.d Configfile.owner_id];
              Xml.tag "DisplayName" [Xml.d Configfile.owner_name];
            ];
          ]
        ]
      ])

  let md5_of_url url =
    U.get_meta url >>= fun lst ->
    return (List.assoc meta_key lst)

  let get_object ~req ~canon bucket path =
    (* TODO: check for .. *)
    (* TODO: get content-type, etag from metadata .. *)
    (* TODO: hash of hashlist to md5 mapping *)
    let content_type = "application/octet-stream" in
    IO.try_catch (fun () ->
      let _, url = url_of_volpath ~canon bucket path in
      IO.try_catch (fun () ->
        md5_of_url url
      ) (fun _ ->
        let digest = ref "" in
        (* there is a race here between calculating the md5 and 
         * downloading the file!
         * this should be done in SXC! *)
        U.with_url_source url (fun source ->
          U.copy (md5_source digest source) ~srcpos:0L `Null
        ) >>= fun () ->
        return !digest
      ) () >>= fun digest ->
      return_source url ~req ~canon ~content_type
          ~etag:digest
    ) (function
      | Unix_error(ENOENT,_,_) | Unix_error(EISDIR,_,_) ->
          (* TODO: is this the correct error message? *)
          return_error Error.NoSuchKey []
      | e -> IO.fail e
    ) ();;

  let fold_entry ~canon bucket prefix delim (fileset, dirset) entry =
    let common_prefix = match delim with
    | Some d ->
        begin try
          let pos = String.index_from entry.U.name (String.length prefix) d in
          Some (String.sub entry.U.name 0 pos)
        with Not_found | Invalid_argument _ -> None
        end
    | None -> None in
    match common_prefix with
    | Some prefix ->
      return (fileset, StringSet.add prefix dirset)
    | None ->
      try_catch (fun () ->
          let _, url = url_of_volpath ~canon bucket entry.U.name in
          md5_of_url url
      ) (fun _ ->
        return "") () >>= fun md5 ->
      let meta=
        entry.U.size, entry.U.mtime, md5 in
      return (StringMap.add entry.U.name meta fileset, dirset)


  let recurse prefix delim dir =
    match delim with
    | Some c ->
      begin try
        let pos = String.index_from dir (String.length prefix) c in
        ignore (String.index_from dir (pos+1) c); (* 2nd occurance of delimiter *)
        false (* don't, we've put a prefix into common-prefixes already *)
      with Not_found | Invalid_argument _ ->
        true
      end
    | None -> true;;

  let get_delim params =
    let delimstr = try List.assoc "delimiter" params with Not_found -> "" in
    match String.length delimstr with
    | 0 -> return None
    | 1 -> return (Some delimstr.[0])
    | _ ->
        return_error Error.NotImplemented ["ListWithLongDelimiter",delimstr];;

  let delim_opt = function
    | None -> []
    | Some s -> [Xml.d (String.make 1 s)]

  let list_bucket ~req ~canon bucket params =
    get_delim params >>= fun delim ->
    let prefix = try List.assoc "prefix" params with Not_found -> "" in
    let base, url = url_of_volpath ~canon bucket prefix in
    let pathprefix = prefix in
    try_catch
      (fun () ->
      U.fold_list ~base url
        ~entry:(fold_entry ~canon bucket pathprefix delim)
        ~recurse:(recurse pathprefix delim)
        (StringMap.empty, StringSet.empty)
      )
      (fun e ->
        U.exists url >>= function
        | Some _ -> fail e
        | None ->
          return_error Error.NoSuchBucket ["Bucket",bucket]
      ) () >>= fun (files, common_prefixes) ->
    begin if files = StringMap.empty then begin
      U.exists base >>= function
        | Some _ -> return ()
        | None ->
          return_error Error.NoSuchBucket ["Bucket",bucket]
    end else return ()
    end >>= fun () ->
    let xml = list_bucket_files2 files common_prefixes in
    return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] (
        Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "ListBucketResult" (
          List.rev_append [
            Xml.tag "Name" [Xml.d bucket];
            Xml.tag "Prefix" [Xml.d prefix];(* TODO: impl these *)
            Xml.tag "Marker" [];
            Xml.tag "Delimiter" (delim_opt delim);
            Xml.tag "MaxKeys" [Xml.d (string_of_int Configfile.max_keys)];
            Xml.tag "IsTruncated" [Xml.d "false"];
          ] xml)
    );;

  open Bucket

  let delete_bucket ~req ~canon bucket =
    IO.try_catch
      (fun () ->
        let base, _ = url_of_volpath ~canon bucket "" in
        U.delete base >>= fun () ->
        return_empty ~req ~canon ~status:`No_content ~reply_headers:[]
      )
      (function
      | Unix.Unix_error(Unix.ENOTEMPTY,_,_) ->
        return_error Error.BucketNotEmpty ["Bucket", bucket]
      | Unix.Unix_error(Unix.ENOENT,_,_) ->
        return_error Error.NoSuchBucket ["Bucket", bucket]
      | e ->
          IO.fail e
      ) ();;

  let delete_object ~req ~canon bucket path =
    IO.try_catch
      (fun () ->
        let _, url = url_of_volpath ~canon bucket path in
        U.delete url >>= fun () ->
        return_empty ~req ~canon ~status:`No_content ~reply_headers:[]
      )
      (function
      | Unix.Unix_error(Unix.ENOENT,_,_) ->
        (* its not an error if its already deleted or it never existed *)
        return_empty ~req ~canon ~status:`No_content ~reply_headers:[]
      | e ->
          IO.fail e
      ) ();;

  let buf = Buffer.create 256

  let mpart_buckets = Hashtbl.create 16

  let mpart_get_bucket ~canon =
    let user = canon.CanonRequest.user in
    try
      return (Hashtbl.find mpart_buckets user)
    with Not_found ->
      let sha1 = Cryptokit.Hash.sha1 () in
      (* must generate same bucket name on all the nodes *)
      sha1#add_string "mpart";
      sha1#add_string user;
      let hex = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) sha1#result in
      let bucket = "libres3-" ^ hex in
      IO.try_catch (fun () ->
        U.create ~replica:1 (fst (url_of_volpath ~canon bucket ""))
      ) (function
        | Unix_error(EEXIST,_,_) -> return ()
        | err -> IO.fail err
      ) () >>= fun () ->
      Hashtbl.add mpart_buckets user bucket;
      return bucket

  let mpart_get_path ~canon bucket path ~uploadId ~part =
    mpart_get_bucket ~canon >>= fun mpart_bucket ->
    let dir = Filename.concat bucket path in
    let mpart_path = Filename.concat dir (Filename.concat uploadId part) in
    return (mpart_bucket, mpart_path)

  let mput_initiate ~canon ~request bucket path =
    Buffer.reset buf;
    Buffer.add_string buf (Digest.string (bucket ^"/"^path));
    Buffer.add_char buf '\x00';
    Buffer.add_string buf (RequestId.to_string canon.CanonRequest.id);
    let uploadId = Cryptoutil.base64url_encode (Buffer.contents buf) in
    mpart_get_path ~canon bucket path ~uploadId ~part:"0" >>= fun (mpart_bucket, mpart_path) ->
    let _, url = url_of_volpath ~canon mpart_bucket mpart_path in
    U.copy (U.of_string "") ~srcpos:0L url >>= fun () ->
    return_xml_canon ~req:request ~canon ~status:`Ok ~reply_headers:[] (
      Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "InitiateMultipartUploadResult"
      [
        Xml.tag "Bucket" [Xml.d bucket];
        Xml.tag "Key" [Xml.d path];
        Xml.tag "UploadId" [Xml.d uploadId]
      ]
    );;

  let validate_partNumber partNumber =
    try
      let n = int_of_string partNumber in
      if n < 1 then
        IO.fail (Failure "too small partNumber")
      else
        return n;
    with Failure _ ->
      return_error Error.InvalidArgument [
        "BadPartNumber",partNumber;
        "Requirements","part numbers must be integers >= 1 and <= 10000"
      ];;

  let mput_part ~canon ~request ~partNumber ~uploadId body bucket path =
    validate_partNumber partNumber >>= fun n ->
    let part = Printf.sprintf "%05d" n in
    mpart_get_path ~canon bucket path ~uploadId ~part >>= fun (mpart_bucket, mpart_path) ->
    put_object ~canon ~request body mpart_bucket mpart_path

  let build_url bucket path =
    (* TODO: use same scheme as the requests, i.e https on https *)
    Neturl.string_of_url (
      Neturl.make_url ~scheme:"http"
      ~host:(!Configfile.base_hostname)
      ~port:(!Configfile.base_port)
      ~path:("" :: bucket :: (List.tl (Neturl.split_path path)))
      CanonRequest.base_syntax
    );;

  let parse_parts lst =
    List.rev (List.rev_map (function
      | `El (((_,"Part"),_),[
        `El (((_,"PartNumber"),_),[`Data part_number]);
        `El (((_,"ETag"),_),[`Data etag]);
        ])
      | `El (((_,"Part"),_),[
        `El (((_,"ETag"),_),[`Data etag]);
        `El (((_,"PartNumber"),_),[`Data part_number]);
        ]) ->
          int_of_string part_number, etag
      | t ->
          failwith ("bad XML:" ^ (CodedIO.Xml.to_string t))
    ) lst)

  let get_part_sizes ~canon bucket path ~uploadId lst =
    IO.rev_map_p (fun (part_number, etag) ->
      let part = Printf.sprintf "%05d" part_number in
      mpart_get_path ~canon bucket path ~uploadId ~part
      >>= fun (mpart_bucket, mpart_path) ->
      let _, url = url_of_volpath ~canon mpart_bucket mpart_path in
      IO.try_catch md5_of_url (function _ ->
        return_error Error.InvalidPart [
          "UploadID", uploadId;
          "part",string_of_int part_number;
          "ExpectedETag",etag;
        ]
      ) url >>= fun digest ->
      let actual_etag = "\"" ^ digest ^ "\"" in
      if actual_etag <> etag then
        return_error Error.InvalidPart [
          "UploadID", uploadId;
          "part",string_of_int part_number;
          "ExpectedETag",etag;
          "ActualETag",actual_etag
        ]
      else
        U.exists url >>= function
        | Some size -> return (size, url)
        | None ->
          return_error Error.InvalidPart [
            "UploadId",uploadId;
            "part",string_of_int part_number
          ]
    ) lst >|= List.fold_left (fun (filesize, names) (size, url) ->
      Int64.add filesize size, url :: names
    ) (0L, [])

  let check_parts ~canon bucket path ~uploadId body =
    parse_input_xml_opt body "CompleteMultipartUpload"
      parse_parts (get_part_sizes ~canon bucket path ~uploadId)

  let list_bucket_uploads l common =
    let _, contents = StringMap.fold (fun name (size, mtime, md5) (lastkey, accum) ->
      let name = Filename.dirname name in
      let key = Filename.dirname name in
      let _, key = Util.url_split_first_component (Neturl.split_path key)
      and uploadId = Filename.basename name in
      let key = String.sub key 1 (String.length key-1) in
      if key = lastkey then lastkey, accum
      else key, (Xml.tag "Upload" [
        Xml.tag "Key" [Xml.d key];
        Xml.tag "UploadId" [Xml.d uploadId];
        Xml.tag "Initiator" fake_owner;
        Xml.tag "Owner" fake_owner;
        Xml.tag "StorageClass" [Xml.d "STANDARD"];
        Xml.tag "Initiated" [Xml.d (
          Util.format_date mtime)
        ]
      ]) :: accum
    ) l ("",[]) in
    StringSet.fold (fun name accum ->
      (Xml.tag "CommonPrefixes" [
        Xml.tag "Prefix" [Xml.d (name ^ "/")]
      ]) :: accum
    ) common contents;;

  let mpart_list ~canon ~req bucket params =
    mpart_get_bucket ~canon >>= fun mpart_bucket ->
    get_delim params >>= fun delim ->
    let prefix = try List.assoc "prefix" params with Not_found -> "" in
    let pathprefix = Filename.concat bucket prefix in
    let base, url = url_of_volpath ~canon mpart_bucket pathprefix in
    try_catch
      (fun () ->
      U.fold_list ~base url
        ~entry:(fold_entry ~canon bucket pathprefix delim)
        ~recurse:(recurse pathprefix delim)
        (StringMap.empty, StringSet.empty)
      )
      (fun e ->
        U.exists url >>= function
        | Some _ -> fail e
        | None ->
          return_error Error.NoSuchBucket ["Bucket",bucket]
      ) () >>= fun (files, common_prefixes) ->
    begin if files = StringMap.empty then begin
      U.exists base >>= function
        | Some _ -> return ()
        | None ->
          return_error Error.NoSuchBucket ["Bucket",bucket]
    end else return ()
    end >>= fun () ->
    let xml = list_bucket_uploads files common_prefixes in
    return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] (
        Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "ListMultipartUploadsResult" (
          List.rev_append [
            Xml.tag "Bucket" [Xml.d bucket];
            Xml.tag "Prefix" [Xml.d prefix];(* TODO: impl these *)
            Xml.tag "KeyMarker" [];
            Xml.tag "NextKeyMarker" [];
            Xml.tag "NextUploadIdMarker" [];
            Xml.tag "Delimiter" (delim_opt delim);
            Xml.tag "IsTruncated" [Xml.d "false"];
            Xml.tag "MaxUploads" [Xml.d (string_of_int Configfile.max_keys)];
          ] xml)
    );;

  let list_parts ~canon ~uploadId bucket path =
    mpart_get_path ~canon bucket path ~uploadId ~part:""
    >>= fun (mpart_bucket, mpart_path) ->
    let base, url = url_of_volpath ~canon mpart_bucket mpart_path in
    U.fold_list ~base url ~entry:(fun (filesize, names) entry ->
      let _, url = url_of_volpath ~canon mpart_bucket entry.U.name in
      U.exists url >>= function
      | Some partsize ->
        return (Int64.add partsize filesize, entry.U.name :: names)
      | None ->
        return_error Error.InvalidPart [
            "UploadId",uploadId;
            "part",entry.U.name
        ]
      ) ~recurse:(fun _ -> true) (0L, []) >|= fun (filesize, names) ->
    mpart_bucket, filesize, List.fast_sort String.compare names

  let mput_list_parts ~canon ~req bucket path ~uploadId =
    mpart_get_path ~canon bucket path ~uploadId ~part:""
    >>= fun (mpart_bucket, mpart_path) ->
    let base, url = url_of_volpath ~canon mpart_bucket mpart_path in
    U.fold_list ~base url ~entry:(fun parts entry ->
        (* TODO: ignore parts that raise errors *)
        let partNumber = int_of_string (Filename.basename entry.U.name) in
        if partNumber > 0 then
          let _, url = url_of_volpath ~canon mpart_bucket entry.U.name in
          IO.try_catch md5_of_url (function _ ->
              return_error Error.InvalidPart [
                "UploadID", uploadId;
                "part",string_of_int partNumber;
              ]) url >>= fun digest ->
          let etag = "\"" ^ digest ^ "\"" in
          return (Xml.tag "Part" [
              Xml.tag "PartNumber" [Xml.d (string_of_int partNumber)];
              Xml.tag "LastModified" [Xml.d (Util.format_date entry.U.mtime)];
              Xml.tag "ETag" [Xml.d etag];
              Xml.tag "Size" [Xml.d (Int64.to_string entry.U.size)]
            ] :: parts)
        else return parts
      ) ~recurse:(fun _ -> true) [] >>= fun parts ->
    (* TODO: support maxParts *)
    (* TODO: check consistency of uploadId with bucket/path *)
    return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] (
      Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "ListPartsResult" (List.rev (List.rev_append parts [
          Xml.tag "StorageClass" [Xml.d "STANDARD"];
          Xml.tag "Owner" fake_owner;
          Xml.tag "Initiator" fake_owner;
          Xml.tag "UploadId" [Xml.d uploadId];
          Xml.tag "Key" [Xml.d (String.sub path 1 (String.length path-1))];
          Xml.tag "Bucket" [Xml.d bucket]
        ])))

  let mput_delete_common ~canon ~request ~uploadId bucket path =
    list_parts ~canon ~uploadId bucket path
    >>= fun (mpart_bucket, _, names) ->
    IO.rev_map_p (fun name ->
      U.delete (snd (url_of_volpath ~canon mpart_bucket name))
    ) names

  let mput_delete ~canon ~request ~uploadId bucket path =
    mput_delete_common ~canon ~request ~uploadId bucket path >>= fun _ ->
    return_empty ~req:request ~canon ~status:`No_content ~reply_headers:[];;

  let rec periodic_send sender got_result msg =
    IO.sleep 10. >>= fun () ->
    if !got_result = true then return ()
    else begin
      S.send_data sender (msg, 0, String.length msg) >>= fun () ->
      periodic_send sender got_result msg
    end

  let spaces = String.make 4096 ' '

  module Result = LRUCacheMonad.ResultT(U.M)

  let periodic_send_until sender msg result_wait =
    let got_result = ref false in
    let _periodic = periodic_send sender got_result spaces in
    result_wait >>= fun result ->
    got_result := true; (* stop sending periodic messages *)
    Result.unwrap result

  module MpartPending = Pendinglimit.Make(U.M)(struct
        type t = string * string
        let compare = Pervasives.compare
  end)
  let mpart_pending = MpartPending.create ()

  let send_long_running ~canon ~req key f =
    MpartPending.bind mpart_pending key (fun _ ->
        let result_wait = Result.lift f () in
        let id = canon.CanonRequest.id
        and id2 = CanonRequest.gen_debug ~canon in
        let headers = add_std_headers ~id ~id2 [] in
        S.send_headers req.server {
          status = `Ok; (* we send 200 with an <Error> in the body if needed *)
          reply_headers = headers;
          last_modified = None;
          content_type = Some "application/xml";
          content_length = None;
          etag = None;
        } >>= fun sender ->
        let decl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" in
        S.send_data sender (decl, 0, String.length decl) >>= fun() ->
        periodic_send_until sender " " result_wait >>= fun result ->
        let str = CodedIO.Xml.to_string ~decl:false result in
        S.send_data sender (str, 0, String.length str)
      )

  let mput_complete ~canon ~request ~uploadId ~body bucket path =
    mpart_get_path ~canon bucket path ~uploadId ~part:"0"
    >>= fun (mpart_bucket, mpart_path) ->
    U.exists (snd (url_of_volpath ~canon mpart_bucket mpart_path)) >>= function
    | None ->
      return_error Error.NoSuchUpload [
        "UploadId",uploadId;
        "bucket",bucket;
        "file",path
      ]
    | Some _ ->
    check_parts ~canon bucket path ~uploadId body >>= fun (filesize, urls) ->
    let key = canon.CanonRequest.user, uploadId in
    send_long_running ~canon ~req:request key (fun url ->
        let _, url = url_of_volpath ~canon bucket path in
        let digestref = ref "" in
        let md5 = Cryptokit.Hash.md5 () in
        U.with_urls_source urls filesize (fun src ->
            let source = md5_source2 md5 src in
            U.copy ~metafn:(md5_metafn md5 digestref) source ~srcpos:0L url >>= fun () ->
            mput_delete_common ~canon ~request ~uploadId bucket path >>= fun _ ->
            return (
              Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "CompleteMultipartUploadResult"
                [
                  Xml.tag "Location" [Xml.d (build_url bucket path)];
                  Xml.tag "Bucket" [Xml.d bucket];
                  Xml.tag "Key" [Xml.d path];
                  Xml.tag "ETag" [Xml.d !digestref];
                ]
            )
          )
    );;

  let list_buckets request canon =
    let base, url = url_of_volpath ~canon "" "" in
    let buckets = ref [] in
    (* TODO: use fold for dirs too *)
    U.fold_list ~base url ~entry:(fun _ _ -> return ())
      ~recurse:(fun dir ->
        begin match validate_bucketname dir (Some "us-standard") with
        | Error.NoError, _ ->
          buckets := dir :: !buckets;
        | _ -> () (* hide volumes with non-S3 compliant names *)
        end;
        false
    ) () >>= fun () ->
    return_xml_canon ~req:request ~canon ~status:`Ok ~reply_headers:[]
      (ServiceOps.list_all_buckets !buckets);;

  let set_object_acl ~canon ~request body _ _ =
    parse_input_xml_opt body "AccessControlPolicy" (function
        | [`El (((_,"Owner"), _), _); (* check owner/id if we really implement this *)
           `El (((_,"AccessControlList"),_), lst)] ->
          Some lst
        | _ ->
          None
      ) (function
        | Some [] ->
          (* grant only owner full-access: treat as no-op,
           * we could reset the acl SX side too! *)
          return_empty ~req:request ~canon ~status:`Ok ~reply_headers:[]
        | _ ->
          return_error Error.AccessDenied ["ACL","Changing ACLs is not supported"]
      )

  let dispatch_request ~request ~canon =
    match
      canon.CanonRequest.req_method, canon.CanonRequest.bucket,
      canon.CanonRequest.path, CanonRequest.actual_query_params canon
    with
    | `GET, Bucket "", "/",[] ->
        list_buckets request canon
    | `GET, Bucket bucket, "/", params
      when List.mem_assoc "uploads" params && List.assoc "uploads" params = "" ->
        mpart_list ~canon ~req:request bucket params
    | `GET, Bucket bucket, "/",params ->
        list_bucket ~req:request ~canon bucket params
    | `HEAD, Bucket bucket, "/",_ ->
        head_bucket ~req:request ~canon bucket
    | `GET, Bucket bucket, path, [] ->
        (* TODO: use params! *)
        get_object ~req:request ~canon bucket path
    | `GET, Bucket _, _, ["acl",""] ->
        send_default_acl ~req:request ~canon
    | `GET, Bucket bucket, path, ["uploadId", uploadId] ->
        mput_list_parts ~canon ~req:request bucket path ~uploadId
    | `GET, Bucket _, _, params ->
        return_error Error.NotImplemented params
    | `HEAD, Bucket bucket, path, _ ->
        get_object ~req:request ~canon bucket path
    | `PUT body, Bucket bucket, "/",[] ->
        create_bucket ~canon ~request body bucket
    | `PUT body, Bucket bucket, path,[] ->
        if Headers.has_header canon.CanonRequest.headers "x-amz-copy-source"
        then
          copy_object ~canon ~request body bucket path
        else
          put_object ~canon ~request body bucket path
    | `PUT body, Bucket bucket, path, ["acl",""] ->
        set_object_acl ~canon ~request body bucket path
    | `POST _, Bucket bucket, path,["uploads",""] ->
        if Headers.has_header canon.CanonRequest.headers "x-amz-copy-source"
        then
          return_error Error.InvalidArgument
          ["InvalidHeader","x-amz-copy-source \
          cannot be used be used when initiating a multipart upload"]
        else
          mput_initiate ~canon ~request bucket path
    | `PUT body, Bucket bucket, path, [
        "partNumber",partNumber;
        "uploadId",uploadId;
        ] ->
          (* TODO: path should be part of uploadId, check that they match! *)
          mput_part ~canon ~request ~partNumber ~uploadId body bucket path
    | `POST body, Bucket bucket, path, ["uploadId",uploadId] ->
        (* TODO: path should be part of uploadId, check that they match! *)
        mput_complete ~canon ~request ~uploadId ~body bucket path
    | `DELETE, Bucket bucket, "/",[] ->
        delete_bucket ~req:request ~canon bucket
    | `DELETE, Bucket bucket, path,[] ->
        delete_object ~req:request ~canon bucket path
    | `DELETE, Bucket bucket, path, ["uploadId", uploadId] ->
        mput_delete ~canon ~request ~uploadId bucket path
    | meth, Bucket bucket, path,params ->
        return_error Error.MethodNotAllowed [
          ("NotImplemented", CanonRequest.string_of_method meth);
          ("Bucket", bucket);
          ("Path", path);
          ("Params", String.concat "||" (List.map (fun (n,v) -> n^"="^v) params))
    ];;

  let is_root_get canon =
    match
      canon.CanonRequest.req_method, canon.CanonRequest.bucket,
      canon.CanonRequest.path, CanonRequest.actual_query_params canon
    with
    | `GET, Bucket "", "/",[] -> true
    | _ -> false

  let validate_authorization ~request ~canon f =
    match CanonRequest.parse_authorization canon with
    | CanonRequest.AuthNone ->
      if is_root_get canon then
        return_string ~req:request
          ~id:canon.CanonRequest.id ~id2:(CanonRequest.gen_debug ~canon)
          ~status:`Ok ~reply_headers:[] ~content_type:"text/html"
          Homepage.root
      else
        return_error Error.AccessDenied ["MissingHeader", "Authorization"]
    | CanonRequest.AuthEmpty ->
        return_error Error.AccessDenied ["MissingHeader", "Authorization"]
    | CanonRequest.AuthMalformed s ->
      return_error Error.InvalidSecurity ["BadAuthorization", s]
    | CanonRequest.AuthDuplicate ->
      return_error Error.InvalidSecurity ["BadAuthorization", "Multiple occurences of Authorization header"]
    | CanonRequest.AuthExpired ->
      return_error Error.ExpiredToken []
    | CanonRequest.Authorization (user, signature) ->
      match !Configfile.sx_host with
      | Some host ->
        let url = Neturl.make_url ~encoded:false ~scheme:"sx" ~host ~path:[""]
          ~user SXC.syntax in
        U.token_of_user (U.of_neturl url) >>= begin function
        | Some hmac_key ->
          let string_to_sign = CanonRequest.string_to_sign canon in
          let expected_signature = Cryptoutil.sign_str hmac_key string_to_sign in
          if expected_signature <> signature then
            return_error Error.SignatureDoesNotMatch [
              ("StringToSign", string_to_sign);
              ("Host", canon.CanonRequest.host);
              ("UndecodedPath", canon.CanonRequest.undecoded_uri_path);
              ("Bucket", Bucket.to_string canon.CanonRequest.bucket);
              ("Hint", "Your S3 secret key should be set to your SX key and your S3 access key should be set to your SX username")
            ]
          else
            f user
        | None ->
            return_error Error.InvalidAccessKeyId [
              "Hint","Your S3 access key must be set to your SX user name"
            ]
        end
      | None -> f ""

  let handle_request_real request =
    let id = RequestId.generate () in
    IO.try_catch (fun () ->
      let meth = map_method request.meth in
      let canon = CanonRequest.canonicalize_request ~id meth request.info in
      let path =
        if canon.CanonRequest.path = "/" then
          "/" ^ (Bucket.to_string  canon.CanonRequest.bucket)
        else
          "/" ^ (Bucket.to_string  canon.CanonRequest.bucket) ^
          canon.CanonRequest.path in
      IO.try_catch (fun () ->
        validate_authorization ~request ~canon (fun user ->
          dispatch_request ~request ~canon:{ canon with CanonRequest.user = user }
        )
      )
      (function
      | Error.ErrorReply (code, details, headers) ->
          return_error_xml
            ~req:request
            ~id2:(CanonRequest.gen_debug ~canon)
            ~id:canon.CanonRequest.id ~path ~headers
            code details
      | Http_client.Http_protocol (Http_client.Timeout e) ->
          return_error_xml
            ~req:request
            ~id2:(CanonRequest.gen_debug ~canon)
            ~id:canon.CanonRequest.id ~path ~headers:[]
            Error.RemoteServiceUnavailable [
              "SXTimeout",e]
      | Http_client.Http_protocol e ->
          return_error_xml
            ~req:request
            ~id2:(CanonRequest.gen_debug ~canon)
            ~id:canon.CanonRequest.id ~path ~headers:[]
            Error.RemoteServiceUnavailable [
              "SXUnavailable",(Printexc.to_string e)]
      | SXIO.Detail (Unix.Unix_error(Unix.EACCES, _, _) as ex, detail) ->
          return_error_xml
            ~req:request
            ~id2:(CanonRequest.gen_debug ~canon)
            ~id:canon.CanonRequest.id ~path ~headers:[]
            Error.AccessDenied (("SXException", (Printexc.to_string ex)) :: detail)
      | SXIO.Detail (Unix.Unix_error _ as ex, detail)->
          return_error_xml
            ~req:request
            ~id2:(CanonRequest.gen_debug ~canon)
            ~id:canon.CanonRequest.id ~path ~headers:[]
            Error.InvalidArgument (("SXException", (Printexc.to_string ex)) :: detail)
      | SXIO.Detail (ex, detail) ->
          return_error_xml
            ~req:request
            ~id2:(CanonRequest.gen_debug ~canon)
            ~id:canon.CanonRequest.id ~path ~headers:[]
            Error.InternalError (("SXException", (Printexc.to_string ex)) :: detail)
      | Unix.Unix_error(Unix.ENOSPC,fn,_)->
          return_error_xml
            ~req:request
            ~id2:(CanonRequest.gen_debug ~canon)
            ~id:canon.CanonRequest.id ~path ~headers:[]
            Error.ServiceUnavailable [
              "OutOfSpace",fn
            ]
      | Ocsigen_stream.Interrupted (Ocsigen_http_com.Lost_connection _) ->
          (* do not send anything back to client, it has already disconnected *)
          fail Ocsigen_http_com.Aborted
      | e ->
        let bt = if Printexc.backtrace_status () then
          ["Backtrace", Printexc.get_backtrace ()]
        else
          [] in
        let str = Printexc.to_string e in
        return_error_xml
            ~req:request
            ~id2:(CanonRequest.gen_debug ~canon)
            ~id:canon.CanonRequest.id ~path ~headers:[]
            Error.InternalError (("Exception", str) :: bt)
      ) ()
    ) (function
    | Ocsigen_http_com.Aborted ->
        fail Ocsigen_http_com.Aborted
    | e ->
      let bt = if Printexc.backtrace_status () then
        ["Backtrace", Printexc.get_backtrace ()]
      else [] in
      return_error_xml ~id2:(CanonRequest.gen_debug2 request.info)
        ~req:request ~id ~path:request.info.CanonRequest.undecoded_url
        ~headers:[]
        Error.InvalidURI (("Exception", Printexc.to_string e) :: bt)
    ) ()
  ;;

  let handle_request _ request =
    begin match debug_output with
    | None -> ()
    | Some ch ->
        (* TODO: lock file? *)
        output_string ch ">>>>Request\n";
        Printf.fprintf ch "\tURL: %s\n" request.info.CanonRequest.undecoded_url;
        Printf.fprintf ch "\tMethod: %s\n"
          (CanonRequest.string_of_method request.meth);
        List.iter (fun (n,v) -> Printf.fprintf ch "\t%s:%s\n" n v)
          request.info.CanonRequest.req_headers;
        output_string ch "----\n";
        flush ch
    end;
    handle_request_real request
  ;;

  let mkdir_maybe dir =
    IO.try_catch (fun () -> IO.mkdir dir 0o700) (function
      | Unix.Unix_error (Unix.EEXIST,_,_) -> return ()
      | e -> IO.fail e
    ) ()
  ;;

  let wsize_float = float_of_int Sys.word_size
  let words_to_kb w =
    ((float_of_int w) *. wsize_float) /. 1024.0;;

  open Gc

  let print_info _ =
    Gc.full_major ();
    let s = Gc.stat () in
    let log = Filename.concat !Paths.log_dir "warnings.log" in
    let f = open_out_gen [Open_append] 0o600 log in
    let pid = Unix.getpid () in
    Printf.fprintf f
      "\nGC stats for %d\n\
      Major heap: %.2f KB used, %.2f KB free, %.2f KB wasted\n\
      Other GC stats:\n\
      minor_words: %f, promoted_words: %f, major_words: %f,\
      minor_collections: %d, major_collections: %d,\
      heap_words: %d, heap_chunks: %d,\
      live_words: %d, live_blocks: %d, free_words: %d, free_blocks: %d,\
      largest_free: %d, fragments: %d, compactions: %d, top_heap_words: %d\n"
      pid
      (words_to_kb s.live_words)
      (words_to_kb s.free_words)
      (words_to_kb s.fragments)
      s.minor_words s.promoted_words s.major_words s.minor_collections
      s.major_collections s.heap_words s.heap_chunks s.live_words
      s.live_blocks s.free_words s.free_blocks s.largest_free s.fragments
      s.compactions s.top_heap_words;
    close_out f
  ;;

  type t = unit
  let init () =
    Printexc.record_backtrace true;
    Sys.set_signal Sys.sigusr2 (Sys.Signal_handle print_info);
    if !Config.secret_access_key = "" && !Configfile.sx_host <> None then
      fail (Failure "SX secret access key must be set!")
    else begin
      Gc.compact ();
      if !Configfile.buckets_dir <> "" && !Configfile.buckets_dir <> "/" then
        mkdir_maybe !Configfile.buckets_dir >>= fun () ->
        mkdir_maybe (Filename.concat !Configfile.buckets_dir "tmp")
        (*      >>= fun () ->  mkdir_maybe (!Configfile.buckets_dir ^ "-meta")*)
      else return ()
    end
  ;;

end;;
