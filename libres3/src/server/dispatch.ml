(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2015 Skylable Ltd. <info-copyright@skylable.com>   *)
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
open Lwt
open SXDefaultIO
module StringMap = Map.Make(String)

let server_name = ("libres3-" ^ Version.version)
type substr = string * int * int

type headers = {
  status: Nethttp.http_status;
  reply_headers: CanonRequest.header list;
  content_type: string option;
  content_length: int64 option;
  last_modified: float option;
  etag_header: string option;
}

module type Server = sig
  type t
  type u
  val log: t -> string -> unit
  val set_user  : t -> string -> unit
  val send_headers: t -> ?body_header:string -> headers -> u Lwt.t
  val send_data: u -> string * int * int -> unit Lwt.t
end

module type Sig = sig
  type source = SXDefaultIO.source
  type server
  type 'a request = {
    server: server;
    body_file: string option;
    info: CanonRequest.request_info;
    meth: 'a;
  } constraint 'a = [> `DELETE | `GET | `HEAD | `POST of source | `PUT of source | `OPTIONS ]

  type t
  val init : unit -> t Lwt.t
  val handle_request: t -> 'a request -> unit Lwt.t
end
module U = SXIO
module IO = EventIO
module Make
    (S: Server)
  : (Sig with type server = S.t
    ) = struct
  type source = SXDefaultIO.source
  type server = S.t
  type ('a) request = {
    server: server;
    body_file: string option;
    info: CanonRequest.request_info;
    meth: 'a;
  } constraint 'a = [> `DELETE | `GET | `HEAD | `POST of source | `PUT of source | `OPTIONS ]
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

  let return_string ~id ~id2 ~req ~status ?last_modified ~reply_headers ~content_type ?body_header str =
    let headers = add_std_headers ~id ~id2 reply_headers ~dbg_body:str in
    let body_header = if req.meth = `HEAD then None else body_header in
    let body_header_len = match body_header with
      | Some s -> String.length s
      | None -> 0 in
    S.send_headers req.server ?body_header {
      status = status;
      reply_headers = headers;
      last_modified = last_modified;
      content_type = Some content_type;
      content_length = Some (Int64.of_int (String.length str + body_header_len));
      etag_header = None;
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
      etag_header = None
    } >>= fun _ -> return ()
  ;;

  let invalid_range ~req ~canon length =
    let header = Headers.make_content_range (`Bytes (None, Some length))
    in
    return_empty ~req ~canon ~status:`Requested_range_not_satisfiable
      ~reply_headers:header

  let parse_ranges headers etag content_length =
    let default_last = Int64.sub content_length 1L in
    match Headers.get_ifrange headers with
    | Some (`Date _) | Some (`Etag (`Weak _)) ->
      (* Date is not a strong-validator in our case:
         we don't know whether the file changed twice during a second,
         cf. RFC7232#section-2.2.2 *)
        None
    | Some (`Etag (`Strong expected_etag)) when etag <> expected_etag ->
      None
    | Some (`Etag (`Strong _)) | None ->
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

  let send_source source ~canon ~first ?length sender =
    if canon.CanonRequest.req_method = `HEAD then return () (* ensure HEAD's body is empty *)
    else
      source.seek ?len:length first >>= fun stream ->
      SXDefaultIO.iter stream (S.send_data sender)
  ;;

  let is_prefix ~prefix str =
    let plen = String.length prefix in
    (String.length str) >= plen &&
    (String.sub str 0 plen) = prefix

  let meta_headers = ["cache-control"; "content-disposition"; "content-encoding"; "expires"]

  let add_meta_headers other_headers xamz_headers =
    List.fold_left (fun accum (key,value) ->
        if is_prefix ~prefix:"x-amz-meta-" key ||
           List.mem key meta_headers then (key, value) :: accum
        else accum
      ) other_headers xamz_headers

  let quote s = "\"" ^ s ^ "\""
  let return_source ~req ~canon ~content_type url ~metalst =
    U.with_url_source url (fun source ->
        let meta = source.meta in
        let size = meta.size and mtime = meta.mtime and etag = meta.etag in
        let headers = add_std_headers ~id:canon.CanonRequest.id
            ~id2:(CanonRequest.gen_debug ~canon) ["ETag", quote etag] in
        let headers = add_meta_headers headers metalst in
        match (parse_ranges canon.CanonRequest.headers etag size) with
        | None ->
          S.send_headers req.server {
            status = `Ok;
            reply_headers = ("Accept-Ranges","bytes") :: headers;
            last_modified = Some mtime;
            content_type = Some content_type;
            content_length = Some size;
            etag_header = Some (etag);
          } >>= send_source source ~canon ~first:0L
        | Some (first, last) as range ->
          if first > last then
            invalid_range ~req ~canon size (* not satisfiable *)
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
              etag_header = None;
            } >>= send_source source ~canon ~first ~length
      ) ;;

  let xml_decl = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"

  let return_xml ?log ~id ~id2 ~req ~status ~reply_headers xml =
    let str = CodedIO.Xml.to_string ~decl:false xml in
    begin match log with
      | None -> ()
      | Some l ->
        l (
          Printf.sprintf "Replying with code %d: %s"
            (Nethttp.int_of_http_status status) str
        )
    end;
    return_string ~id ~id2 ~req ~status ~reply_headers
      ~content_type:"application/xml" ~body_header:xml_decl str;;

  let return_xml_canon ?log ~req ~canon ~status ~reply_headers xml =
    return_xml ?log ~id:canon.CanonRequest.id ~id2:(CanonRequest.gen_debug ~canon)
      ~req ~status ~reply_headers xml;;

  let str_of_method = function
    | `DELETE -> "DELETE"
    | `GET -> "GET"
    | `HEAD -> "HEAD"
    | `POST _ -> "POST"
    | `PUT _ -> "PUT"
    | `OPTIONS -> "OPTIONS"
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
    fail (Error.ErrorReply (code, details, []));;

  let map_method = function
    | (`DELETE | `GET | `HEAD | `POST _| `PUT _ | `OPTIONS) as a -> a
    | _ -> `UNSUPPORTED

  let max_input_xml = 5*1048576
  let max_input_mem = 65536L

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

  let is_mpart_bucket bucket =
    String.length bucket = 48 &&
    String.sub bucket 0 8 = "libres3-"

  let validate_bucketname ?(hide=false) bucket region =
    if hide && is_mpart_bucket bucket then
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
    Lwt.catch (fun () ->
        let base, _ = url_of_volpath ~canon bucket "" in
        U.create base >>= fun () ->
        return_empty ~req ~canon ~status:`Ok ~reply_headers:["Location","/"^bucket]
      ) (function
        | Unix_error(EEXIST,_,_) ->
          return_empty ~req ~canon ~status:`Ok ~reply_headers:["Location","/"^bucket]
        | err ->
          fail err
      );;

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
        match validate_bucketname ~hide:true bucket region with
        | Error.NoError, [] ->
          make_bucket ~req:request ~canon bucket
        | error, detail ->
          return_error error detail
      );;

  let head_bucket ~req ~canon bucket =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      return_empty ~req ~canon ~status:`Ok ~reply_headers:[]
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]
  ;;

  let canon_id_of_name name =
    let full = String.make 64 '\x00' in
    String.blit name 0 full 0 (String.length name);
    Cryptokit.transform_string (Cryptokit.Hexa.encode ()) full

  let owner name =
    [
      (* TODO: use real uid once SX supports it *)
      Xml.tag "ID" [Xml.d (canon_id_of_name name)];
      Xml.tag "DisplayName" [ Xml.d name]
    ]

  module StringSet = Set.Make(String)
  let list_bucket_files2 limit owner_name l common =
    let is_truncated,length,contents = StringMap.fold (fun name (size, mtime, etag) (is_truncated,n, accum) ->
        match limit with
        | Some lim when n = lim -> true,n, accum
        | _ ->
        false, n+1,
        (Xml.tag "Contents" [
            Xml.tag "Key" [Xml.d name];
            Xml.tag "LastModified" [Xml.d (
                Util.format_date mtime)
              ];
            Xml.tag "ETag" [Xml.d (quote etag)];
            Xml.tag "Size" [Xml.d (Int64.to_string size)];
            Xml.tag "StorageClass" [Xml.d "STANDARD"];
            Xml.tag "Owner" (owner owner_name)
          ]) :: accum
      ) l (false,0,[]) in
    is_truncated,length,StringSet.fold (fun name accum ->
        (Xml.tag "CommonPrefixes" [
            Xml.tag "Prefix" [Xml.d (name ^ "/")]
          ]) :: accum
      ) common contents;;

  (* returns tmpfile, digest *)
  let source_of_request ~canon body =
    let source = (
      Headers.field_single_value canon.CanonRequest.headers
        "x-amz-copy-source" "") in
    let source_bucket, source_path =
      Util.url_split_first_component (Neturl.split_path source) in
    let decoded_path = CanonRequest.uri_decode source_path in
    let _, url = url_of_volpath ~canon source_bucket decoded_path in
    U.with_url_source url (fun source -> return source.meta) >>= fun meta ->
    return (url, meta.mtime, meta.etag)

  let hash_stream2 hash stream () =
    stream () >>= fun (str,pos,len) ->
    if len > 0 then
      hash#add_substring str pos len;
    return (str, pos, len);;

  let hash_seek_source2 hash source ?len pos =
    source.seek ?len pos >>= fun stream ->
    return (hash_stream2 hash stream);;

  let hash_source2 hash source =
    `Source {
      meta = source.meta;
      seek = hash_seek_source2 hash source
    };;

  let fd_seek fd ?len pos =
    IO.lseek fd pos >>= fun () ->
    return (fun () ->
        let buf = String.make Config.buffer_size '\x00' in
        IO.really_read fd buf 0 (String.length buf) >>= fun amount ->
        return (buf, 0, amount)
      )

  let filter_sha256 input f =
    if input.meta.size <= max_input_mem then
      read_all ~max:(Int64.to_int input.meta.size) ~input >>= fun str ->
      let sha256 = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) str in
      f (U.of_string str) ~sha256
    else
      IO.with_tempfile (fun tmpfd ->
          let sha256 = Cryptokit.Hash.sha256 () in
          U.copy (hash_source2 sha256 input) ~srcpos:0L (U.of_sink (fun pos ->
              IO.lseek tmpfd pos >>= fun () ->
              return (fun (buf, pos, len) -> IO.really_write tmpfd buf pos len)))
          >>= fun () ->
          IO.lseek tmpfd 0L >>= fun () ->
          let source = U.of_source {
              meta = input.meta;
              seek = fd_seek tmpfd
            } in
          f source ~sha256:sha256#result
        );;

  let meta_key = "libres3-etag-md5"
  let meta_key_size = "libres3-filesize"
  let meta_key_content_type = "libres3-content-type"

  let default_mime_type = "binary/octet-stream"
  let content_type canon =
    if canon.CanonRequest.content_type = "" then
      [meta_key_content_type, default_mime_type]
    else [meta_key_content_type, canon.CanonRequest.content_type]

  let compute_meta ~canon url =
    match Headers.field_single_value canon.CanonRequest.headers
            "x-amz-metadata-directive" "COPY" with
    | "COPY" ->
      U.get_meta url
    | "REPLACE" ->
      Lwt.catch (fun () ->
          U.get_meta url >>= fun metalst ->
          return [meta_key, List.assoc meta_key metalst]
        ) (fun _ -> return []) >>= fun etag_meta ->
      (* keep libres3-specific metadata *)
      return (List.rev_append (List.rev_append etag_meta (content_type canon))
                (canon.CanonRequest.headers.Headers.ro#fields))
    | d ->
      return_error Error.InvalidRequest ["Invalid-x-amz-metadata-directive", d]

  let copy_tourl ~canon body url =
    source_of_request ~canon body >>= fun (source, mtime, etag) ->
    (* TODO: check for copying onto self and allow only if meta is set to
       REPLACE *)
    compute_meta ~canon source >>= fun metalst ->
    U.copy source ~srcpos:0L ~metafn:(fun () -> metalst) url >>= fun () ->
    (* TODO: check for .., check Content-MD5, store it,
     * and store Content-Type*)
    return (etag, mtime);;

  let copy_object ~canon ~request body bucket path =
    (* TODO: check that body is empty *)
    let _, url = url_of_volpath ~canon bucket path in

    (* TODO: handle the other x-amz-copy* and x-amz-meta* directives too *)
    Lwt.catch
      (fun () ->
         copy_tourl ~canon body url
      )
      (function
        | Unix.Unix_error(Unix.EISDIR,_,_) ->
          (* COPY of directory is not supported by S3 *)
          return_error Error.NoSuchKey []
        | Unix.Unix_error(Unix.ENOENT,_,_) ->
          return_error Error.NoSuchKey ["Key",path]
        | e -> fail e
      ) >>= fun (etag, lastmodified) ->
    return_xml_canon ~req:request ~canon ~status:`Ok ~reply_headers:[] (
      Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "CopyObjectResult" [
        Xml.tag "LastModified" [Xml.d (Util.format_date lastmodified)];
        Xml.tag "ETag" [Xml.d (quote etag)]
      ]
    );;

  let put_metafn ~canon md5 digestref size () =
    digestref := Cryptokit.transform_string (Cryptokit.Hexa.encode ())
        md5#result;
    let xamz_headers = canon.CanonRequest.headers.Headers.ro#fields in
    let libres3_keys = (meta_key, !digestref) ::
                       (meta_key_size, Int64.to_string size) ::
                       content_type canon in
    add_meta_headers libres3_keys xamz_headers

  let put_object ~canon ~request src bucket path =
    let md5 = Cryptokit.Hash.md5 () in
    let source = hash_source2 md5 src in
    Lwt.catch
      (fun () ->
         let _, url = url_of_volpath ~canon bucket path in
         let digestref = ref "" in
         U.copy ~metafn:(put_metafn ~canon md5 digestref src.meta.size) source ~srcpos:0L url >>= fun () ->
         return_empty ~canon ~req:request ~status:`Ok
           ~reply_headers:["ETag",quote !digestref]
      )
      (function
        | Unix.Unix_error(Unix.ENOENT,_,bucket) ->
          return_error Error.NoSuchBucket ["Bucket",bucket]
        | e ->
          fail e
      )
  ;;

  let find_owner acl =
    let _, id, _ = (List.find (fun (_, _, perm) -> List.mem `Owner perm) acl) in
    id

  let get_owner ~canon bucket =
    U.get_acl (fst (url_of_volpath ~canon bucket "")) >>= fun acl ->
    try
      let `UserName owner = find_owner acl in
      return owner
    with Not_found ->
      (* we cannot determine the owner unless we are the owner or an admin *)
      return "libres3-default"

  let canonical_id = function
    | `UserName displayname ->
      [
        Xml.tag "ID" [Xml.d (canon_id_of_name displayname)];
        Xml.tag "DisplayName" [Xml.d displayname]
      ]

  let map_permission = function
    | [`Read] -> ["READ"]
    | [`Write] -> ["WRITE"]
    | l when List.mem `Owner l && List.mem `Read l && List.mem `Write l ->
      ["FULL_CONTROL"]
    | l when List.mem `Read l && List.mem `Write l ->
      ["READ";"WRITE"]
    | l when List.mem `Manager l -> ["READ_ACP"; "WRITE_ACP"]
    | _ -> failwith "TODO: unknown permissions"

  let libres3_all_users = "libres3-all-users"
  let uri_all_users = "http://acs.amazonaws.com/groups/global/AllUsers"

  let user2uri = [
    libres3_all_users, uri_all_users;
  ]

  let uri2user = List.rev_map (fun (a, b) -> (b, a)) user2uri

  let grantee_of_id = function
    | `UserName name as id ->
      begin try
          Xml.tag ~attrs:[
            Xml.attr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance";
            Xml.attr "xsi:type" "Group"
          ] "Grantee" [
            Xml.tag "URI" [ Xml.d (List.assoc name user2uri) ]
          ]
        with Not_found ->
          Xml.tag ~attrs:[
            Xml.attr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance";
            Xml.attr "xsi:type" "CanonicalUser"
          ] "Grantee" (canonical_id id)
      end

  let map_grant (_, id, perm) : CodedIO.Xml.t list =
    (* TODO: detect special users and map back *)
    List.rev_map (fun (s3_perm:string) ->
        Xml.tag "Grant" [
          grantee_of_id id;
          Xml.tag "Permission" [Xml.d s3_perm]
        ]) (map_permission perm)

  let is_real (_,`UserName name,_) = name <> libres3_all_users

  let send_default_acl ~req ~canon bucket =
    U.get_acl (fst (url_of_volpath ~canon bucket "")) >>= fun acl ->
    try
      let owner = find_owner acl in
      let acl = List.filter is_real acl in
      return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] (
        Xml.tag "AccessControlPolicy" [
          Xml.tag "Owner" (canonical_id owner);
          Xml.tag "AccessControlList" (List.flatten (List.rev_map map_grant acl))
        ])
    with Not_found ->
      return_error Error.AccessDenied ["LibreS3ErrorMessage","You are not the owner"]

  let get_attr ns attr attrs =
    snd (List.find (fun ((a_ns, a_name), _) ->
        a_ns = ns && a_name = attr) attrs)

  let id_of_canonical id =
    try
      let name = Cryptokit.transform_string (Cryptokit.Hexa.decode ()) id in
      try
        String.sub name 0 (String.index name '\x00')
      with Not_found -> name
    with _ -> "id:" ^ id

  let map_acl_type = function
    | "CanonicalUser", (`El (((_, "ID"), _), [`Data id])) :: _ ->
      `UserName (id_of_canonical id)
    | "AmazonCustomerByEmail",
      [`El (((_,"EmailAddress"),_), [`Data email])] ->
      failwith "Email address not supported"
    | "Group",
      [`El (((_,"URI"),_), [`Data uri])] ->
      `UserName (List.assoc uri uri2user)
    | _ -> failwith "Bad grantee type"

  let map_permission = function
    | "READ" -> [ `Read ]
    | "WRITE" -> [ `Write ]
    | "READ_ACP" -> [ `Manager ]
    | "WRITE_ACP" -> [ `Manager ]
    | "FULL_CONTROL" -> [ `Owner; `Manager; `Read; `Write]
    | _ -> failwith "Unknown permission"

  let map_acl x =  match x with
    | `El (((_, "Grant"), _), [
        `El (((_, "Grantee"), flags), grantee);
        `El (((_, "Permission"),_), [`Data perm])]) ->
      `Grant, map_acl_type
        ((get_attr "http://www.w3.org/2001/XMLSchema-instance" "type" flags),
         grantee), map_permission perm
    | #CodedIO.Xml.t ->
      failwith "Bad xml" (* TODO: map to MalformedACLError *)

  let canon_acl_map (op, user, acl) = op, user, List.stable_sort Pervasives.compare acl

  let canon_acl a =
    List.stable_sort Pervasives.compare (List.rev_map canon_acl_map a)

  let is_acl_equivalent a b =
    (canon_acl a) = (canon_acl b)

  let set_noop_acl ~canon ~request body bucket =
    (* TODO: also look at x-amz-acl* headers *)
    parse_input_xml_opt body "AccessControlPolicy" (function
        | [`El (((_,"Owner"), _), _); (* check owner/id if we really implement this *)
           `El (((_,"AccessControlList"),_), lst)] ->
          Some lst
        | _ ->
          None
      ) (function
        | Some l ->
          let url, _ = url_of_volpath ~canon bucket "" in
          U.get_acl url >>= fun current_acl ->
          let new_acl = List.rev_map map_acl l in
          (* We cannot set ACL because in SX we only have a single ACL for entire
             bucket and all objects inside it, whereas S3 has ACL for bucket operations and object
             operations *)
          if is_acl_equivalent current_acl new_acl then
            return_empty ~req:request ~canon ~status:`Ok ~reply_headers:[]
          else
            return_error Error.NotImplemented
              ["LibreS3ErrorMessage", "Use bucket policies instead"]
        | _ ->
          return_error Error.MalformedACLError []
      )

  let return_json ~req ~canon ~status ~reply_headers json =
    return_string ~id:canon.CanonRequest.id ~id2:(CanonRequest.gen_debug ~canon)
      ~content_type:"application/json"
      ~req ~status ~reply_headers (CodedIO.Json.to_string json)

  let get_bucket_policy ~req ~canon bucket =
    U.get_acl (fst (url_of_volpath ~canon bucket "")) >>= fun acl ->
    let has_anon_read = List.exists (fun (_,`UserName name,_) ->
        name = libres3_all_users) acl in
    if has_anon_read then
      let policy = Policy.json_of_policy (Policy.build_anon_policy bucket) in
      return_json ~req ~canon ~status:`Ok ~reply_headers:[] policy
    else
      return_error Error.NotSuchBucketPolicy []

  let md5_of_url url =
    U.get_meta url >>= fun lst ->
    return (Int64.of_string (List.assoc meta_key_size lst),
            List.assoc meta_key lst)

  let mime_assoc =
    (* load system mime.types, and fallback to internal if not available *)
    let types =
      try Ocsigen_charset_mime.parse_mime_types ~filename:"/etc/mime.types"
      with _ -> Ocsigen_charset_mime.default_mime_assoc () in
    Ocsigen_charset_mime.set_default_mime types default_mime_type

  let get_object ~req ~canon bucket path =
    (* TODO: check for .. *)
    (* TODO: hash of hashlist to md5 mapping *)
    Lwt.catch (fun () ->
        let _, url = url_of_volpath ~canon bucket path in
        U.get_meta url >>= fun metalst ->
        let content_type =
          try List.assoc meta_key_content_type metalst
          with Not_found ->
            Ocsigen_charset_mime.find_mime path mime_assoc in
        return_source url ~req ~canon ~content_type ~metalst
      ) (function
        | Unix_error((ENOENT|EISDIR),_,_)
        | Detail (Unix_error((ENOENT|EISDIR),_,_), _) ->
          (* TODO: is this the correct error message? *)
          return_error Error.NoSuchKey []
        | e -> fail e
      );;

  let fold_entry ~canon bucket prefix delim marker (fileset, dirset) entry =
    if Some entry.name = marker then return (fileset, dirset)
    else
      let common_prefix = match delim with
        | Some d ->
          begin try
              let pos = String.index_from entry.name (String.length prefix) d in
              Some (String.sub entry.name 0 pos)
            with Not_found | Invalid_argument _ -> None
          end
        | None -> None in
      match common_prefix with
      | Some prefix when Some prefix = marker -> return (fileset, dirset)
      | Some prefix ->
        return (fileset, StringSet.add prefix dirset)
      | None ->
        let etag = entry.etag in
        let meta=
          entry.size, entry.mtime, etag in
        return (StringMap.add entry.name meta fileset, dirset)


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

  let index_bucket ~req ~canon bucket path =
    let base, url = url_of_volpath ~canon bucket path in
    U.exists url >>= fun exists ->
    let path = Util.string_before_suffix path ".sxnewdir" in
    let base, url = url_of_volpath ~canon bucket path in
    let dirs = ref [] in
    U.fold_list ~no_recurse:true ~base url
      ~entry:(fun accum entry -> return (("/" ^ entry.name, Some entry) :: accum))
      ~recurse:(fun _ -> false)
      [] >>= fun entries ->
    if entries = [] && not exists then
      return_string ~id:canon.CanonRequest.id ~id2:(CanonRequest.gen_debug ~canon)
        ~content_type:"text/plain"
        ~req ~status:`Not_found ~reply_headers:[] "Not found"
    else
      return_string ~id:canon.CanonRequest.id ~id2:(CanonRequest.gen_debug ~canon)
        ~content_type:"text/html; charset=utf-8"
        ~req ~status:`Ok ~reply_headers:[]
        (DirectoryListing.html_of_list bucket path (List.rev_append !dirs entries))

  let list_bucket ~req ~canon bucket params =
    get_delim params >>= fun delim ->
    let prefix = try List.assoc "prefix" params with Not_found -> "" in
    let marker = try Some (List.assoc "marker" params) with Not_found -> None in
    let base, url = url_of_volpath ~canon bucket prefix in
    let pathprefix = prefix in
    let no_recurse = delim = Some '/' in
    let limit =
      try Some (min (int_of_string (List.assoc "max-keys" params)) Config.maxkeys)
      with Not_found ->
        if delim = None then Some Config.maxkeys else None in
    Lwt.catch
      (fun () ->
         U.fold_list ~base ~no_recurse ?limit ?marker url
           ~entry:(fold_entry ~canon bucket pathprefix delim marker)
           ~recurse:(recurse pathprefix delim)
           (StringMap.empty, StringSet.empty)
      )
      (fun e ->
         U.exists url >>= function
         | true -> fail e
         | false ->
           return_error Error.NoSuchBucket ["Bucket",bucket]
      ) >>= fun (files, common_prefixes) ->
    begin if files = StringMap.empty then begin
        U.exists base >>= function
        | true -> return ()
        | false ->
          return_error Error.NoSuchBucket ["Bucket",bucket]
      end else return ()
    end >>= fun () ->
    get_owner ~canon bucket >>= fun owner ->
    let is_truncated, files, rev_xml = list_bucket_files2 limit owner files common_prefixes in
    let xml = List.rev rev_xml in
    let maxkeys = if delim = None then files else Config.maxkeys in
    return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] (
      Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "ListBucketResult" (
        List.rev_append [
          Xml.tag "Name" [Xml.d bucket];
          Xml.tag "Prefix" [Xml.d prefix];(* TODO: impl these *)
          Xml.tag "Marker" (match marker with | Some m -> [Xml.d m] | None -> []);
          Xml.tag "Delimiter" (delim_opt delim);
          Xml.tag "MaxKeys" [Xml.d (string_of_int maxkeys)];
          Xml.tag "IsTruncated" [if is_truncated then Xml.d "true" else Xml.d "false"];
        ] xml)
    );;

  open Bucket

  let delete_bucket ~req ~canon bucket =
    Lwt.catch
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
          fail e
      );;

  let delete_object ~req ~canon bucket path =
    Lwt.catch
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
          fail e
      );;

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
      Lwt.catch (fun () ->
          U.create ~replica:1 (fst (url_of_volpath ~canon bucket ""))
        ) (function
          | Unix_error(EEXIST,_,_) -> return ()
          | err -> fail err
        ) >>= fun () ->
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
    let default_meta = content_type canon in
    let meta = add_meta_headers default_meta canon.CanonRequest.headers.Headers.ro#fields in
    U.copy (U.of_string "") ~metafn:(fun () -> meta) ~srcpos:0L url >>= fun () ->
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
        fail (Failure "too small partNumber")
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
    (* TODO: we don't have to calculate MD5 here *)
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
    Lwt_list.rev_map_p (fun (part_number, etag) ->
        let part = Printf.sprintf "%05d" part_number in
        mpart_get_path ~canon bucket path ~uploadId ~part
        >>= fun (mpart_bucket, mpart_path) ->
        let _, url = url_of_volpath ~canon mpart_bucket mpart_path in
        Lwt.catch (fun () -> md5_of_url url) (function _ ->
            return_error Error.InvalidPart [
              "UploadID", uploadId;
              "part",string_of_int part_number;
              "ExpectedETag",etag;
            ]
          ) >>= fun (size, digest) ->
        let actual_etag = quote digest in
        if actual_etag <> etag then
          return_error Error.InvalidPart [
            "UploadID", uploadId;
            "part",string_of_int part_number;
            "ExpectedETag",etag;
            "ActualETag",actual_etag
          ]
        else
          let `Url u = url in return (size, u)
      ) lst >|= List.fold_left (fun (min_partsize, filesize, names) (size, url) ->
        min min_partsize size, Int64.add filesize size, url :: names
      ) (Int64.max_int, 0L, [])

  let check_parts ~canon bucket path ~uploadId body =
    parse_input_xml_opt body "CompleteMultipartUpload"
      parse_parts (get_part_sizes ~canon bucket path ~uploadId)

  let list_bucket_uploads owner_name l common =
    let contents = StringMap.fold (fun name (size, mtime, md5) (accum) ->
        let name = Filename.dirname name in
        let key = Filename.dirname name in
        let _, key = Util.url_split_first_component (Neturl.split_path key)
        and uploadId = Filename.basename name in
        let key = String.sub key 1 (String.length key-1) in
        (Xml.tag "Upload" [
            Xml.tag "Key" [Xml.d key];
            Xml.tag "UploadId" [Xml.d uploadId];
            Xml.tag "Initiator" (owner owner_name);
            Xml.tag "Owner" (owner owner_name);
            Xml.tag "StorageClass" [Xml.d "STANDARD"];
            Xml.tag "Initiated" [Xml.d (
                Util.format_date mtime)
              ]
          ]) :: accum
      ) l ([]) in
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
    Lwt.catch
      (fun () ->
         U.fold_list ~base url
           ~entry:(fold_entry ~canon bucket pathprefix delim None)
           ~recurse:(recurse pathprefix delim)
           (StringMap.empty, StringSet.empty)
      )
      (fun e ->
         U.exists url >>= function
         | true -> fail e
         | false ->
           return_error Error.NoSuchBucket ["Bucket",bucket]
      ) >>= fun (files, common_prefixes) ->
    begin if files = StringMap.empty then begin
        U.exists base >>= function
        | true -> return ()
        | false ->
          return_error Error.NoSuchBucket ["Bucket",bucket]
      end else return ()
    end >>= fun () ->
    U.get_acl (fst (url_of_volpath ~canon mpart_bucket "")) >>= fun acl ->
    let `UserName owner = find_owner acl in
    let xml = list_bucket_uploads owner files common_prefixes in
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
          Xml.tag "MaxUploads" [Xml.d (string_of_int Config.maxkeys)];
        ] xml)
    );;

  let list_parts ~canon ~uploadId bucket path =
    mpart_get_path ~canon bucket path ~uploadId ~part:""
    >>= fun (mpart_bucket, mpart_path) ->
    let base, url = url_of_volpath ~canon mpart_bucket mpart_path in
    U.fold_list ~base url ~entry:(fun names entry ->
        return (entry.name :: names)
      ) ~recurse:(fun _ -> true) [] >|= fun names ->
    mpart_bucket, List.fast_sort String.compare names

  let mput_list_parts ~canon ~req bucket path ~uploadId =
    mpart_get_path ~canon bucket path ~uploadId ~part:""
    >>= fun (mpart_bucket, mpart_path) ->
    let base, url = url_of_volpath ~canon mpart_bucket mpart_path in
    U.fold_list ~base url ~entry:(fun parts entry ->
        (* TODO: ignore parts that raise errors *)
        let partNumber = int_of_string (Filename.basename entry.name) in
        if partNumber > 0 then
          let _, url = url_of_volpath ~canon mpart_bucket entry.name in
          Lwt.catch (fun () -> md5_of_url url) (function _ ->
              return_error Error.InvalidPart [
                "UploadID", uploadId;
                "part",string_of_int partNumber;
              ]) >>= fun (_, etag) ->
          return (Xml.tag "Part" [
              Xml.tag "PartNumber" [Xml.d (string_of_int partNumber)];
              Xml.tag "LastModified" [Xml.d (Util.format_date entry.mtime)];
              Xml.tag "ETag" [Xml.d (quote etag)];
              Xml.tag "Size" [Xml.d (Int64.to_string entry.size)]
            ] :: parts)
        else return parts
      ) ~recurse:(fun _ -> true) [] >>= fun parts ->
    (* TODO: support maxParts *)
    (* TODO: check consistency of uploadId with bucket/path *)
    get_owner ~canon mpart_bucket >>= fun owner_name ->
    return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] (
      Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "ListPartsResult" (List.rev (List.rev_append parts [
          Xml.tag "StorageClass" [Xml.d "STANDARD"];
          Xml.tag "Owner" (owner owner_name);
          Xml.tag "Initiator" (owner owner_name);
          Xml.tag "UploadId" [Xml.d uploadId];
          Xml.tag "Key" [Xml.d (String.sub path 1 (String.length path-1))];
          Xml.tag "Bucket" [Xml.d bucket]
        ])))

  let mput_delete_common ~canon ~request ~uploadId bucket path =
    list_parts ~canon ~uploadId bucket path
    >>= fun (mpart_bucket, names) ->
    Lwt_list.rev_map_p (fun name ->
        U.delete ~async:true (snd (url_of_volpath ~canon mpart_bucket name))
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

  module Result = LRUCacheMonad.ResultT(Lwt)

  let periodic_send_until sender msg result_wait =
    let got_result = ref false in
    let _periodic = periodic_send sender got_result spaces in
    result_wait >>= fun result ->
    got_result := true; (* stop sending periodic messages *)
    Result.unwrap result

  module MpartPending = Pendinglimit.Make(Lwt)(struct
      type t = string * string
      let compare = Pervasives.compare
    end)
  let mpart_pending = MpartPending.create ()

  let send_long_running_uncached ~canon ~req f _ =
    let result_wait = Result.lift f () in
    let id = canon.CanonRequest.id
    and id2 = CanonRequest.gen_debug ~canon in
    let headers = add_std_headers ~id ~id2 [] in
    S.send_headers req.server ~body_header:xml_decl {
      status = `Ok; (* we send 200 with an <Error> in the body if needed *)
      reply_headers = headers;
      last_modified = None;
      content_type = Some "application/xml";
      content_length = None;
      etag_header = None;
    } >>= fun sender ->
    periodic_send_until sender " " result_wait >>= fun result ->
    let str = CodedIO.Xml.to_string ~decl:false result in
    S.send_data sender (str, 0, String.length str)

  let send_long_running ~canon ~req key f =
    MpartPending.bind mpart_pending key
      (send_long_running_uncached ~canon ~req f)

  let parse_multi_delete lst =
    let quiet = ref false in
    let result = List.rev_map (function
        | `El (((_,"Object"),_),
               (`El (((_,"Key"),_), [`Data key]) :: _ )) ->
          Some key
        | `El (((_, "Quiet"), _), [`Data quietstr]) ->
          if quietstr <> "true" then
            failwith ("bad value for quiet: " ^ quietstr);
          quiet := true;
          None
        | t -> failwith ("bad XML:" ^ (CodedIO.Xml.to_string t))
      ) lst in
    !quiet, result

  let multi_delete_ok quiet path =
    if quiet then return None
    else return (Some (Xml.tag "Deleted" [
        Xml.tag "Key" [Xml.d path]
      ]))

  let multi_delete_error path e =
    return (Some (Xml.tag "Error" [
        Xml.tag "Key" [Xml.d path];
        (* TODO: reuse code/message mapping from main handler *)
        Xml.tag "Code" [Xml.d "InternalError"];
        Xml.tag "Message" [Xml.d (Printexc.to_string e)]
      ]))

  let perform_multi_delete ~req ~canon bucket (quiet, lst) =
    send_long_running_uncached ~canon ~req (fun () ->
        Lwt_list.rev_map_p (function
            | None -> return None
            | Some path ->  Lwt.catch (fun () ->
                let _, url = url_of_volpath ~canon bucket path in
                U.delete url >>= fun () ->
                multi_delete_ok quiet path
              ) (function
                | Unix.Unix_error(Unix.ENOENT,_,_) ->
                  S.log req.server (Printf.sprintf "ENOENT when deleting %s/%s"
                                      bucket path);
                  multi_delete_ok quiet path (* ENOENT is considered deleted according to docs *)
                | e ->
                  multi_delete_error path e
              )) lst >>= fun result ->
        let result = List.fold_left (fun accum -> function
            | Some (e:Xml.t) -> e :: accum
            | None -> accum) [] result in
        return (Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "DeleteResult" result)
      ) ()

  let multi_delete_objects ~canon ~request bucket ~body =
    let base, _ = url_of_volpath ~canon bucket "/" in
    U.exists base >>= function
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]
    | true ->
      let perform = perform_multi_delete ~canon ~req:request bucket in
      parse_input_xml_opt body "Delete" parse_multi_delete perform

  let mput_complete ~canon ~request ~uploadId ~body bucket path =
    mpart_get_path ~canon bucket path ~uploadId ~part:"0" >>= fun (mpart_bucket, mpart_path) ->
    Lwt.catch (fun () ->
        U.get_meta (snd (url_of_volpath ~canon mpart_bucket mpart_path))
        >>= fun metalst ->
        check_parts ~canon bucket path ~uploadId body >>= fun (min_partsize, filesize, urls) ->
        let key = canon.CanonRequest.user, uploadId in
        let _, url = url_of_volpath ~canon bucket path in
        let etag = uploadId ^ "-1" in
        let content_type = List.assoc meta_key_content_type metalst in
        let meta = add_meta_headers [meta_key, etag;
                                     meta_key_content_type, content_type] metalst in
        let quotaok_wait, quotaok_wake = Lwt.task () in
        let result =
          Lwt.finalize (fun () ->
              Lwt.catch (fun () ->
                  U.copy ~quotaok:(fun () -> Lwt.wakeup quotaok_wake ()) ~metafn:(fun () -> meta) (`Urls (urls, filesize)) ~srcpos:0L url)
                (fun exn ->
                   Lwt.wakeup_exn quotaok_wake exn;
                   fail exn
                )
            ) (fun () ->
              mput_delete_common ~canon ~request ~uploadId bucket path >>= fun _ -> return_unit)
        in
        quotaok_wait >>= fun () ->
          send_long_running ~canon ~req:request key (fun url ->
            result >>= fun () ->
            return (
              Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "CompleteMultipartUploadResult"
                [
                  Xml.tag "Location" [Xml.d (build_url bucket path)];
                  Xml.tag "Bucket" [Xml.d bucket];
                  Xml.tag "Key" [Xml.d path];
                  Xml.tag "ETag" [Xml.d (quote etag)];
                ]
            )
          )
      ) (function
        | Unix.Unix_error(Unix.ENOENT,_,_) ->
          return_error Error.NoSuchUpload [
            "UploadId",uploadId;
            "bucket",bucket;
            "file",path
          ]
        | e -> fail e
      )

  let list_all_buckets all owner_name =
    Xml.tag ~attrs:[Xml.attr "xmlns" Configfile.reply_ns] "ListAllMyBucketsResult" [
      Xml.tag "Owner" (owner owner_name);
      Xml.tag "Buckets" (
        List.rev_map (fun name ->
            Xml.tag "Bucket" [
              Xml.tag "Name" [Xml.d name];
              Xml.tag "CreationDate" [
                (* TODO: SX should send ctime! *)
                Xml.d (Util.format_date 0.)
              ]
            ]
          ) all
      )
    ];;

  let list_buckets request canon =
    let base, url = url_of_volpath ~canon "" "" in
    let buckets = ref [] in
    (* TODO: use fold for dirs too *)
    U.fold_list ~base url ~entry:(fun _ _ -> return ())
      ~recurse:(fun dir ->
          begin match validate_bucketname ~hide:true dir (Some "us-standard") with
            | Error.NoError, _ ->
              buckets := dir :: !buckets;
            | _ -> () (* hide volumes with non-S3 compliant names *)
          end;
          false
        ) () >>= fun () ->
    let self = canon.CanonRequest.user in
    Lwt_list.rev_map_p (fun bucket ->
        get_owner ~canon bucket >>= fun owner_name ->
        return (if !Configfile.show_all_volumes || owner_name = self then bucket else "")
      ) !buckets >>= fun buckets ->
    let buckets = List.filter (fun s -> s <> "") buckets in
    return_xml_canon ~req:request ~canon ~status:`Ok ~reply_headers:[]
      (list_all_buckets buckets self);;


  let create_special_users ~canon () =
    List.fold_left (fun accum name ->
        accum >>= fun _ ->
        U.create_user (fst (url_of_volpath ~canon "" "")) name)
      (return "") [ libres3_all_users ]

  let delete_bucket_policy ~canon ~request bucket =
    create_special_users ~canon () >>= fun _ ->
    let anon_read = `Revoke, `UserName libres3_all_users, [`Read] in
    U.set_acl (fst (url_of_volpath ~canon bucket "")) [anon_read]

  let set_bucket_policy ~canon ~request body bucket =
    read_all ~input:body ~max:max_input_xml >>= fun json ->
    try
      let policy = Policy.of_string json in
      if Policy.valid policy bucket then
        if Policy.is_anon_bucket_policy policy bucket then
          create_special_users ~canon () >>= fun _ ->
          let anon_read = `Grant, `UserName libres3_all_users, [`Read] in
          U.set_acl (fst (url_of_volpath ~canon bucket "")) [anon_read]
        else
          return_error Error.NotImplemented
            ["LibreS3ErrorMessage",
             "Only supported policy is \"Read-Only Permission to an Anonymous User\""]
      else
        return_error Error.InvalidPolicyDocument [ "Policy is not valid", "" ]
    with
    | CodedIO.Json.Error s ->
      return_error Error.InvalidPolicyDocument [ "JSON format error", s ]
    | s ->
      return_error Error.InvalidPolicyDocument
        ["Failed to parse policy", Printexc.to_string s]

  let known_api (name, _) = match name with
    | "acl" | "cors" | "delete" | "lifecycle" | "location" | "logging"
    | "notification" | "policy" | "requestPayment" | "tagging" | "torrent"
    | "uploadId" | "uploads" | "versioning" | "versions" | "website" -> true
    | _ -> false

  let is_s3_index canon =
    match
      canon.CanonRequest.req_method, canon.CanonRequest.bucket,
      canon.CanonRequest.path, CanonRequest.actual_query_params canon
    with
    | (`GET | `HEAD), Bucket _, path, [] ->
      let path = Util.string_before_suffix path ".sxnewdir" in
      String.length path > 0 &&
      path.[String.length path-1] = '/'
    | _ -> false

  (* stubs *)
  let delete_stub ~req ~canon bucket param =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      IO.try_finally (fun () ->
          S.log req.server (Printf.sprintf "Stub DELETE for %s/?%s" bucket param);
          mpart_get_bucket ~canon >>= fun mpart_bucket ->
          let path = bucket ^ "-" ^ param in
          let _, url = url_of_volpath ~canon mpart_bucket path in
          U.delete ~async:true url)
        (fun () ->
           return_empty ~req ~canon ~status:`No_content ~reply_headers:[]) ()
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]

  let get_stub ~req ~canon bucket param ~root default =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      mpart_get_bucket ~canon >>= fun mpart_bucket ->
      let path = bucket ^ "-" ^ param in
      let _, url = url_of_volpath ~canon mpart_bucket path in
      Lwt.catch (fun () ->
          U.with_url_source url (fun input -> read_all ~input ~max:max_input_xml) >|= Xml.parse_string)
        (fun _ ->
           return (Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] root default)) >>= fun xml ->
      S.log req.server (Printf.sprintf "Stub GET for %s/?%s" bucket param);
      return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] xml
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]

  let get_bucket_lifecycle ~req ~canon bucket param =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      mpart_get_bucket ~canon >>= fun mpart_bucket ->
      let path = bucket ^ "-" ^ param in
      let _, url = url_of_volpath ~canon mpart_bucket path in
      Lwt.catch (fun () ->
          U.with_url_source url (fun input -> read_all ~input ~max:max_input_xml) >>= fun xml ->
          return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] (Xml.parse_string xml))
        (fun _ ->
           return_error Error.NoSuchLifecycleConfiguration ["Bucket", bucket ])
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]

  let put_stub ~req ~canon ~body bucket param =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      mpart_get_bucket ~canon >>= fun mpart_bucket ->
      read_all ~input:body ~max:max_input_xml >>= begin function
        | "" -> return ()
        | str ->
          try
            S.log req.server (Printf.sprintf "Stub PUT for %s/?%s: %s" bucket param str);
            ignore (Xml.parse_string str);
            let path = bucket ^ "-" ^ param in
            let _, url = url_of_volpath ~canon mpart_bucket path in
            U.copy (U.of_string str) ~srcpos:0L url >>= fun () ->
            return ()
          with Xmlm.Error ((line,col), err) ->
            return_error Error.MalformedXML [
              "ErrorLine", (string_of_int line);
              "ErrorColumn", (string_of_int col);
              "ErrorMessage", (Xmlm.error_message err)
            ]
      end >>= fun () ->
      return_empty ~req ~canon ~status:`Ok ~reply_headers:[]
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]

  let get_cors ~req ~canon bucket path =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      return_error Error.AccessDenied
        ["Bucket", bucket; "LibreS3ErrorMessage", "CORS is not enabled"]
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]

  let get_bucket_location ~req ~canon bucket =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      return_xml_canon ~req ~canon ~status:`Ok ~reply_headers:[] (
        (* US classic region *)
        Xml.tag ~attrs:[Xml.attr "xmlns" reply_ns] "LocationConstraint" [])
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]

  let get_bucket_cors ~req ~canon bucket =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      return_error Error.NoSuchCORSConfiguration [ "BucketName", bucket ]
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]

  let get_bucket_website ~req ~canon bucket =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      return_error Error.NoSuchWebsiteConfiguration [ "BucketName", bucket ]
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]

  let get_bucket_replication ~req ~canon bucket =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      return_error Error.NoSuchReplicationConfiguration [ "BucketName", bucket ]
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]

  let get_bucket_tagging ~req ~canon bucket =
    let _, url = url_of_volpath ~canon bucket "" in
    U.exists url >>= function
    | true ->
      return_error Error.NoSuchTagSetError [ "BucketName", bucket ]
    | false ->
      return_error Error.NoSuchBucket ["Bucket", bucket]


  let dispatch_request ~request ~canon expires =
    match expires with
    | Some e when e < Unix.gettimeofday () ->
      return_error Error.ExpiredToken []
    | _ ->
      match
        canon.CanonRequest.req_method, canon.CanonRequest.bucket,
        canon.CanonRequest.path, CanonRequest.actual_query_params canon
      with
      | _, Bucket bucket, _, _ when is_mpart_bucket bucket ->
        return_error Error.AccessDenied
          ["Bucket", bucket; "LibreS3ErrorMessage","This bucketname is reserved for internal use"]

      (* public bucket indexing *)
      | `GET, Bucket bucket, path, [] when canon.CanonRequest.user = libres3_all_users && is_s3_index canon ->
        index_bucket ~req:request ~canon bucket path

      (* Service GET API *)
      | `GET, Bucket "", "/",[] ->
        list_buckets request canon

      (* Bucket DELETE APIs *)
      | `DELETE, Bucket bucket, "/",[] ->
        delete_bucket ~req:request ~canon bucket
      | `DELETE, Bucket bucket, "/", ["cors" as param, ""] ->
        delete_stub ~canon ~req:request bucket param
      | `DELETE, Bucket bucket, "/", ["lifecycle" as param, ""] ->
        delete_stub ~canon ~req:request bucket param
      | `DELETE, Bucket bucket, "/", ["policy", ""] ->
        delete_bucket_policy ~canon ~request bucket
      | `DELETE, Bucket bucket, "/", ["replication" as param, ""] ->
        delete_stub ~canon ~req:request bucket param
      | `DELETE, Bucket bucket, "/", ["tagging" as param, ""] ->
        delete_stub ~canon ~req:request bucket param
      | `DELETE, Bucket bucket, "/", ["website" as param, ""] ->
        delete_stub ~canon ~req:request bucket param

      (* Bucket GET APIs *)
      | `GET, Bucket bucket, "/", ["acl",""] ->
        send_default_acl ~req:request ~canon bucket
      | `GET, Bucket bucket, "/", ["cors",""] ->
        get_bucket_cors ~req:request ~canon bucket
      | `GET, Bucket bucket, "/", ["lifecycle" as param,""] ->
        get_bucket_lifecycle ~req:request ~canon bucket param
      | `GET, Bucket bucket, "/", ["policy",""] ->
        get_bucket_policy ~req:request ~canon bucket
      | `GET, Bucket bucket, "/", ["location",""] ->
        get_bucket_location ~req:request ~canon bucket
      | `GET, Bucket bucket, "/", ["logging" as param,""] ->
        get_stub ~req:request ~canon bucket param ~root:"BucketLoggingStatus" []
      | `GET, Bucket bucket, "/", ["notification" as param,""] ->
        get_stub ~req:request ~canon bucket param ~root:"NotificationConfiguration" []
      | `GET, Bucket bucket, "/", ["replication",""] ->
        get_bucket_replication ~req:request ~canon bucket
      | `GET, Bucket bucket, "/", ["tagging",""] ->
        get_bucket_tagging ~req:request ~canon bucket
      | `GET, Bucket bucket, "/", (["versions" ,""] as params) ->
        return_error Error.NotImplemented params
      | `GET, Bucket bucket, "/", ["requestPayment" as param,""] ->
        get_stub ~req:request ~canon bucket param ~root:"RequestPaymentConfiguration"
          [ Xml.tag "Payer" [Xml.d "BucketOwner"] ]
      | `GET, Bucket bucket, "/", ["versioning" as param,""] ->
        get_stub ~req:request ~canon bucket param ~root:"VersioningConfiguration" []
      | `GET, Bucket bucket, "/", ["website",""] ->
        get_bucket_website ~req:request ~canon bucket
      | `GET, Bucket bucket, "/", params
        when List.mem_assoc "uploads" params && List.assoc "uploads" params = "" ->
        mpart_list ~canon ~req:request bucket params
      | `GET, Bucket bucket, "/", params ->
        list_bucket ~req:request ~canon bucket params

      (* Bucket HEAD API *)
      | `HEAD, Bucket bucket, "/",_ ->
        head_bucket ~req:request ~canon bucket

      (* Bucket PUT APIs *)
      | `PUT body, Bucket bucket, "/",[] ->
        create_bucket ~canon ~request body bucket
      | `PUT body, Bucket bucket, "/", ["acl",""] ->
        set_noop_acl ~request ~canon body bucket
      | `PUT body, Bucket bucket, "/", ["cors" as param,""] ->
        put_stub ~req:request ~canon ~body bucket param
      | `PUT body, Bucket bucket, "/", ["lifecycle" as param,""] ->
        put_stub ~req:request ~canon ~body bucket param
      | `PUT body, Bucket bucket, "/", ["policy",""] ->
        set_bucket_policy ~canon ~request body bucket
      | `PUT body, Bucket bucket, "/", ["logging" as param,""] ->
        put_stub ~req:request ~canon ~body bucket param
      | `PUT body, Bucket bucket, "/", ["notification" as param,""] ->
        put_stub ~req:request ~canon ~body bucket param
      | `PUT body, Bucket bucket, "/", ["replication" as param,""] ->
        put_stub ~req:request ~canon ~body bucket param
      | `PUT body, Bucket bucket, "/", ["tagging" as param,""] ->
        put_stub ~req:request ~canon ~body bucket param
      | `PUT body, Bucket bucket, "/", ["requestPayment" as param,""] ->
        put_stub ~req:request ~canon ~body bucket param
      | `PUT body, Bucket bucket, "/", ["versioning" as param,""] ->
        put_stub ~req:request ~canon ~body bucket param
      | `PUT body, Bucket bucket, "/", ["website" as param,""] ->
        put_stub ~req:request ~canon ~body bucket param

      (* Object APIs *)
      | `DELETE, Bucket bucket, path,[] ->
        delete_object ~req:request ~canon bucket path
      | `POST body, Bucket bucket, "/", ["delete", ""] ->
        multi_delete_objects ~canon ~request bucket ~body

      | `GET, Bucket bucket, path, params when not (List.exists known_api params) ->
        (* TODO: use params! *)
        get_object ~req:request ~canon bucket path

      | `GET, Bucket bucket, _, params when List.mem_assoc "acl" params && List.assoc "acl" params = "" ->
        (* TODO: versioning *)
        send_default_acl ~req:request ~canon bucket

      | `GET, Bucket bucket, _, (["torrent",""] as params) ->
        return_error Error.NotImplemented params

      | `HEAD, Bucket bucket, path, _ ->
        (* TODO: versioning *)
        get_object ~req:request ~canon bucket path

      | `OPTIONS, Bucket bucket, path, _ ->
        get_cors ~req:request ~canon bucket path

      | `POST body, Bucket bucket, path, ([] as params) ->
        return_error Error.NotImplemented params
      | `POST body, Bucket bucket, path, (["restore", ""] as params) ->
        return_error Error.NotImplemented params

      | `PUT body, Bucket bucket, path,[] ->
        (* TODO: versioning *)
        if Headers.has_header canon.CanonRequest.headers "x-amz-copy-source"
        then
          copy_object ~canon ~request body bucket path
        else
          put_object ~canon ~request body bucket path

      | `PUT body, Bucket bucket, _, ["acl",""] ->
        set_noop_acl ~request ~canon body bucket

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
        (* TODO: versioning *)
        (* TODO: path should be part of uploadId, check that they match! *)
        mput_complete ~canon ~request ~uploadId ~body bucket path

      | `DELETE, Bucket bucket, path, ["uploadId", uploadId] ->
        mput_delete ~canon ~request ~uploadId bucket path

      | `GET, Bucket bucket, path, ["uploadId", uploadId] ->
        mput_list_parts ~canon ~req:request bucket path ~uploadId

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

  let is_s3_get_object canon =
    match
      canon.CanonRequest.req_method, canon.CanonRequest.bucket,
      canon.CanonRequest.path, CanonRequest.actual_query_params canon
    with
    | (`GET | `HEAD), Bucket _, path, params ->
      path <> "" && path <> "/" && not (List.exists known_api params)
    | _ -> false

  let empty_sha256 = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) ""

  let rec return_error_signature tries ~request ~canon url lst =
    if tries = 0 then begin
      U.invalidate_token_of_user (U.of_neturl url);
      validate_authorization (tries+1) ~request ~canon
    end else
      return_error Error.SignatureDoesNotMatch lst

  and validate_authorization tries ~request ~canon =
    match CanonRequest.parse_authorization canon with
    | CanonRequest.AuthNone ->
      if is_root_get canon then
        return_string ~req:request
          ~id:canon.CanonRequest.id ~id2:(CanonRequest.gen_debug ~canon)
          ~status:`Ok ~reply_headers:[] ~content_type:"text/html"
          Homepage.root
      else if is_s3_get_object canon ||  (!Configfile.allow_public_bucket_index && is_s3_index canon) then
        Lwt.catch (fun () ->
            dispatch_request None ~request
              ~canon:{ canon with CanonRequest.user = libres3_all_users })
          (function
            | Error.ErrorReply (Error.NoSuchBucket, _, _) ->
              return_error Error.AccessDenied ["MissingHeader", "Authorization"]
            | e -> fail e)
      else if is_s3_index canon then
        return_error Error.AccessDenied ["MissingHeader", "Authorization"; "LibreS3ErrorMessage", "Directory indexing is not allowed: add allow_public_bucket_index=true to libres3.conf to enable it"]
      else
        return_error Error.AccessDenied ["MissingHeader", "Authorization"]
    | CanonRequest.AuthEmpty ->
      return_error Error.AccessDenied ["MissingHeader", "Authorization"]
    | CanonRequest.AuthMalformed s ->
      return_error Error.InvalidSecurity ["BadAuthorization", s]
    | CanonRequest.AuthDuplicate ->
      return_error Error.InvalidSecurity ["BadAuthorization", "Multiple occurences of Authorization header"]
    | CanonRequest.AuthorizationV4 (auth, signature,expires) ->
      let credential = auth.CanonRequest.credential in
      let user = credential.CanonRequest.keyid in
      S.set_user request.server user;
      begin match !Configfile.sx_host with
        | Some host ->
          let url = Neturl.make_url ~encoded:false ~scheme:"sx" ~host ~path:[""]
              ~user SXC.syntax in
          U.token_of_user (U.of_neturl url) >>= begin function
            | Some hmac_key ->
              (* TODO: body *)
              let f ~sha256 ~canon =
                let canonical_request, string_to_sign =
                  CanonRequest.string_to_sign_v4 auth ~sha256 ~canon_req:canon in
                let expected_signature =
                  CanonRequest.sign_string_v4 ~key:hmac_key credential string_to_sign in
                if expected_signature <> signature then
                  return_error_signature tries ~request ~canon url [
                    ("StringToSign", string_to_sign);
                    ("CanonicalRequest", canonical_request);
                    ("Host", canon.CanonRequest.host);
                    ("UndecodedPath", canon.CanonRequest.undecoded_uri_path);
                    ("Bucket", Bucket.to_string canon.CanonRequest.bucket);
                    ("Hint", "Your S3 secret key should be set to your SX key and your S3 access key should be set to your SX username")
                  ]
                else
                  dispatch_request ~request
                    ~canon:{ canon with CanonRequest.user = user } expires
              in
              begin match canon.CanonRequest.req_method with
                | `PUT body ->
                  filter_sha256 body (fun (`Source input) ~sha256 ->
                      f ~sha256 ~canon:{ canon with CanonRequest.req_method = `PUT input })
                | `POST body ->
                  filter_sha256 body (fun (`Source input) ~sha256 ->
                      f ~sha256 ~canon:{ canon with CanonRequest.req_method = `POST input })
                | _ -> f ~sha256:empty_sha256 ~canon
              end
            | None ->
              return_error Error.InvalidAccessKeyId [
                "Hint","Your S3 access key must be set to your SX user name";
                "AccessKeyID", user
              ]
          end
        | None ->
          dispatch_request ~request
            ~canon:{ canon with CanonRequest.user = "" } expires
      end
    | CanonRequest.Authorization (user, signature, expires) ->
      S.set_user request.server user;
      match !Configfile.sx_host with
      | Some host ->
        let url = Neturl.make_url ~encoded:false ~scheme:"sx" ~host ~path:[""]
            ~user SXC.syntax in
        U.token_of_user (U.of_neturl url) >>= begin function
          | Some hmac_key ->
            let string_to_sign = CanonRequest.string_to_sign canon in
            let expected_signature = Cryptoutil.sign_str hmac_key string_to_sign in
            if expected_signature <> signature then
              return_error_signature tries ~request ~canon url [
                ("StringToSign", string_to_sign);
                ("Host", canon.CanonRequest.host);
                ("UndecodedPath", canon.CanonRequest.undecoded_uri_path);
                ("Bucket", Bucket.to_string canon.CanonRequest.bucket);
                ("Hint", "Your S3 secret key should be set to your SX key and your S3 access key should be set to your SX username")
              ]
            else
              dispatch_request ~request
                ~canon:{ canon with CanonRequest.user = user } expires
          | None ->
            return_error Error.InvalidAccessKeyId [
              "Hint","Your S3 access key must be set to your SX user name"
            ]
        end
      | None ->
        dispatch_request ~request
          ~canon:{ canon with CanonRequest.user = user } expires

  let handle_request_real request =
    let id = RequestId.generate () in
    Lwt.catch (fun () ->
        let meth = map_method request.meth in
        let canon = CanonRequest.canonicalize_request ~id meth request.info in
        let path =
          if canon.CanonRequest.path = "/" then
            "/" ^ (Bucket.to_string  canon.CanonRequest.bucket)
          else
            "/" ^ (Bucket.to_string  canon.CanonRequest.bucket) ^
            canon.CanonRequest.path in
        Lwt.catch (fun () ->
            validate_authorization 0 ~request ~canon
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
            | Detail (Http_client.Http_protocol e, detail) ->
              return_error_xml
                ~req:request
                ~id2:(CanonRequest.gen_debug ~canon)
                ~id:canon.CanonRequest.id ~path ~headers:[]
                Error.RemoteServiceUnavailable (
                ("SXUnavailable",(Printexc.to_string e)) :: detail)
            | Detail (Unix.Unix_error(Unix.EACCES, _, _) as ex, detail) ->
              return_error_xml
                ~req:request
                ~id2:(CanonRequest.gen_debug ~canon)
                ~id:canon.CanonRequest.id ~path ~headers:[]
                Error.AccessDenied (("SXException", (Printexc.to_string ex)) :: detail)
            | Detail (Unix.Unix_error(Unix.ENOSPC, _, _), detail) ->
              return_error_xml
                ~req:request
                ~id2:(CanonRequest.gen_debug ~canon)
                ~id:canon.CanonRequest.id ~path ~headers:[]
                Error.EntityTooLarge detail
            | Detail (Unix.Unix_error _ as ex, detail)->
              return_error_xml
                ~req:request
                ~id2:(CanonRequest.gen_debug ~canon)
                ~id:canon.CanonRequest.id ~path ~headers:[]
                Error.InvalidArgument (("SXException", (Printexc.to_string ex)) :: detail)
            | Detail (ex, detail) ->
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
            | Ocsigen_stream.Interrupted (Ocsigen_http_com.Lost_connection _)
            | Lwt.Canceled ->
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
          )
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
      )
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
      let path = Filename.concat !Paths.log_dir "access.log" in
      Lwt.catch (fun () -> Accesslog.reopen ~path ())
        (fun exn ->
           EventLog.warning ~exn "cannot open access.log";
           return_unit)
    end
  ;;

end;;
