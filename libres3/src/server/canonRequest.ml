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

open Netstring_str

type ('a,'b) methods = [ `DELETE | `GET | `HEAD | `POST of 'a | `PUT of 'b | `OPTIONS | `UNSUPPORTED ]
type header = string * string
type request_info = {
  req_headers: header list;
  undecoded_url: string;
}

module StringMap = Map.Make(String)
type ('a,'b) t = {
  req_method: ('a,'b) methods;
  bucket: Bucket.t;
  lpath: string;
  path: string;
  orig_uri: string;
  content_md5: string;
  content_type: string;
  date: float;
  date_header: string;
  host: string;
  expires: string;
  undecoded_uri_path: string;
  headers: Headers.t;
  query_params_multi: Headers.StringSet.t StringMap.t;
  query_params: string StringMap.t;
  id: RequestId.t;
  user : string;
  is_virtual_hosted: bool
}


let x_amz_re = regexp_case_fold "x-amz-.*"

let string_of_method = function
  | `DELETE -> "DELETE"
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `POST _ -> "POST"
  | `PUT _ -> "PUT"
  | `OPTIONS -> "OPTIONS"
  | _ -> "N/A";;

let canonicalized_amz_headers c =
  let sorted_lowercased_amz = Headers.filter_names (fun name ->
      (* TODO: its already lowercase, use starts_with *)
      (string_match x_amz_re name 0) <> None
    ) c.headers in
  (* TODO: replace folding white-space with single-space? *)
  let combined =
    List.map (fun name ->
        name ^ ":" ^ (String.concat "," (Headers.field_values c.headers name)) ^ "\n"
      ) sorted_lowercased_amz in
  String.concat "" combined;;

let prefix_bucket b =
  let s = Bucket.to_string b in
  if s = "" then ""
  else "/" ^ s;;

let stringmap_all map =
  (* can't use Map.bindings, because OCaml 3.11 doesn't have it *)
  StringMap.fold (fun key data accum ->
      (key, data) :: accum
    ) map [];;

let compare_nameval (name1,_) (name2,_) = String.compare name1 name2
let canonicalized_resource c =
  (* TODO: this is an approximation, do we need the real
   * un-decoded URI? *)
  (* TODO: strip bucket if it starts with it, and stop at '?'*)
  let filtered_subresources = List.filter (fun (n,_) ->
      (* TODO: would it be more efficient to take the difference
       * of two maps than this linear comparison?*)
      n = "acl" || n = "cors" || n = "lifecycle" || n = "location" || n = "logging" ||
      n = "notification" || n = "partNumber" || n = "policy" ||
      n = "requestPayment" || n = "torrent" || n = "uploadId" ||
      n = "replication" || n = "tagging" ||
      n = "delete" || n = "uploads" || n = "versionId" || n = "versioning" || n = "versions" ||
      n = "website") (stringmap_all c.query_params) in
  let sorted_subresources = List.fast_sort compare_nameval filtered_subresources
  in
  let subresources =
    if sorted_subresources = [] then "" else
      "?" ^ (String.concat "&" (
          List.map (fun (n,v) ->
              if v = "" then n else n ^ "=" ^ v
            ) sorted_subresources
        )) in
  (prefix_bucket c.bucket) ^ c.undecoded_uri_path ^ subresources

let split_query_re = regexp_string "&"
let split_param_re = regexp_string "="

let hex2int = function
  | '0'..'9' as c -> Char.code c - Char.code '0'
  | 'a'..'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A'..'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> invalid_arg ""

(* best effort decoder, don't fail if string is not properly encoded *)
let rec decode_loop b s pos =
  if pos = String.length s then Buffer.contents b
  else match s.[pos] with
    | '+' ->
      Buffer.add_char b ' ';
      decode_loop b s (pos+1)
    | '%' when pos+2 < String.length s ->
      let next =
        try
          let c = Char.chr (((hex2int s.[pos+1]) lsl 4) lor (hex2int s.[pos+2])) in
          Buffer.add_char b c;
          pos+3
        with Invalid_argument _ ->
          Buffer.add_char b '%';
          pos+1 in
      decode_loop b s next
    | c ->
      Buffer.add_char b c;
      decode_loop b s (pos+1)

let uri_decode s =
  let b = Buffer.create (String.length s) in
  decode_loop b s 0

let parse_query encoded_query =
  try
    let params = Netstring_str.split split_query_re encoded_query in
    List.fold_left (fun accum nameval ->
        let name, value =
          match Netstring_str.bounded_split split_param_re nameval 2 with
          | name :: value :: [] ->
            uri_decode name, uri_decode value
          | name :: [] ->
            uri_decode name, ""
          | _ ->
            raise (Error.ErrorReply(Error.InvalidURI,["querypart",nameval],[]))
        in
        let old  =
          try StringMap.find name accum
          with Not_found ->  Headers.StringSet.empty in
        StringMap.add name (Headers.StringSet.add value old) accum) StringMap.empty params
  with
  | Not_found -> StringMap.empty

let base_syntax =
  { (Hashtbl.find Neturl.common_url_syntax "http")
    with Neturl.url_accepts_8bits = true
  };;

let get_query_param_opt q param =
  try
    StringMap.find param q
  with Not_found -> ""
;;

let header_overrides = ["x-amz-date","Date"]

let transform_path = function
  | "" | "/" -> "/"
  | path when path.[String.length path-1]='/' ->
    (* SX strips trailing /, and also forbids trailing . or .. *)
    path ^ ".sxnewdir"
  | path -> path

let merge s =
  match Headers.StringSet.elements s with
  | [ one ] -> one
  | multi -> String.concat "\x00" multi

let canonicalize_request ~id req_method
    {req_headers=req_headers; undecoded_url=undecoded_url} =
  let headers = Headers.build header_overrides req_headers in
  let host = Headers.field_single_value headers "host" !Configfile.base_hostname in
  let hurl = Neturl.parse_url ~base_syntax (
      "http://" ^ host) in
  (* client may not encode some characters that OCamlnet would reject as unsafe,
   * for example [.
   * fixup_url_string doesn't help with [ either.
   * Do the safe thing: decode the URL path, and encode it again *)
  let is_virtual_hosted, bucket, undecoded_uri_path = Bucket.from_url hurl undecoded_url in
  let decoded = Neturl.norm_path (Neturl.split_path
                                    (uri_decode undecoded_uri_path)) in
  let absolute_url = Neturl.modify_url hurl ~path:decoded ~encoded:false in
  let query =
    try
      let qpos = String.index undecoded_url '?' in
      let len = (String.length undecoded_url) - qpos - 1 in
      if len > 0 then
        String.sub undecoded_url (qpos+1) len
      else ""
    with Not_found -> "" in
  let query_params_multi = parse_query query in
  let query_params = StringMap.map merge query_params_multi in
  let lpath = Neturl.join_path (Neturl.url_path ~encoded:false absolute_url) in
  let path = transform_path lpath in
  {
    req_method = req_method;
    content_md5 = Headers.field_single_value headers "Content-MD5" "";
    content_type = Headers.field_single_value headers "Content-Type" "";
    date = Headers.get_date headers;
    date_header = Headers.orig_field_value headers "Date";
    expires = get_query_param_opt query_params "Expires";
    orig_uri = undecoded_url;
    host = host;
    bucket = bucket;
    headers = headers;
    lpath  = lpath;
    path = path;
    undecoded_uri_path = undecoded_uri_path;
    query_params_multi = query_params_multi;
    query_params = query_params;
    id = id;
    user = "";
    is_virtual_hosted = is_virtual_hosted
  };;

let string_to_sign canon_req =
  let b = Buffer.create Configfile.small_buffer_size in
  let add s =
    Buffer.add_string b s;
    Buffer.add_char b '\n' in 
  add (string_of_method canon_req.req_method);
  add canon_req.content_md5;
  add canon_req.content_type;
  if Headers.has_header canon_req.headers "authorization" then
    add canon_req.date_header
  else
    add canon_req.expires;
  Buffer.add_string b (canonicalized_amz_headers canon_req);
  Buffer.add_string b (canonicalized_resource canon_req);
  Buffer.contents b;;

(* implement encoding exactly as in AWS4 signatures *)
let uri_encode ?(encode_slash=true) input =
  let b = Buffer.create Configfile.small_buffer_size in
  String.iter (fun ch ->
      if (ch >= 'A' && ch <= 'Z') ||
         (ch >= 'a' && ch <= 'z') ||
         (ch >= '0' && ch <= '9') ||
         ch == '_' || ch == '-' || ch == '~' || ch == '.' then
        Buffer.add_char b ch
      else if ch = '/' && not encode_slash then
        Buffer.add_char b ch
      else begin
        Printf.bprintf b "%%%02X" (Char.code ch)
      end
    ) input;
  Buffer.contents b

let uri_encode_params l =
  String.concat "&" (List.rev (List.rev_map (fun (k, vl) ->
      let key = uri_encode k in
      let lst = match Headers.StringSet.elements vl with
        | [] -> [""]
        | l -> l in
      String.concat "&" (List.rev (List.rev_map (fun v ->
          key ^ "=" ^ (uri_encode v)) lst))
    ) l))

let starts_with s with_ =
  let n = String.length with_ in
  String.length s >= n && String.sub s 0 n = with_

let must_sign_header header =
  header = "host" (*|| header = "content-type" *)||
  starts_with header "x-amz-"

let trim v = v (* already trimmed? *)

let canonical_headers canon_req signed_headers =
  let headers = canon_req.headers in
  String.concat "\n" (List.rev (List.rev_map (fun header ->
      let v = String.concat ","
          (List.sort String.compare (Headers.field_values headers header)) in
      header ^ ":" ^ (trim v)
    ) signed_headers))

type credential_v4 = {
  keyid: string;
  date_ymd: string;
  region: string;
  service: string;
}

type auth_v4 = {
  signed_headers: Headers.StringSet.t;
  credential: credential_v4;
  auth_date: float;
  payloadhash: string option;
}

let scope_of_credential c =
  String.concat "/" [ c.date_ymd; c.region; c.service; "aws4_request" ]

let string_to_sign_v4 auth ?sha256 ~canon_req =
  let absolute_uri =
    if canon_req.is_virtual_hosted then canon_req.lpath
    else "/" ^(Bucket.to_string canon_req.bucket) ^ canon_req.lpath
  in
  let signed_headers =
    Headers.StringSet.elements (Headers.StringSet.union auth.signed_headers
                                  (Headers.filter_names_set must_sign_header canon_req.headers)) in
  let payloadhash = match auth.payloadhash, sha256 with
    | Some s, _ -> s
    | None, None -> "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"
    | None, Some s -> Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s in
  let params = StringMap.remove "X-Amz-Signature" canon_req.query_params_multi
  in
  let canonical_request = String.concat "\n" [
      string_of_method canon_req.req_method;
      uri_encode ~encode_slash:false absolute_uri;
      uri_encode_params (StringMap.bindings params);
      canonical_headers canon_req signed_headers;
      "";
      String.concat ";" signed_headers;
      payloadhash
    ] in
  canonical_request,
  String.concat "\n" [
    "AWS4-HMAC-SHA256";
    Util.format_date_iso8601_timestamp auth.auth_date;
    scope_of_credential auth.credential;
    Cryptoutil.sha256 canonical_request
  ]

let signing_key_v4 key credentials =
  let d_key = Cryptoutil.hmac_sha256 ("AWS4" ^ key) credentials.date_ymd in
  let dr_key = Cryptoutil.hmac_sha256 d_key credentials.region in
  let drs_key = Cryptoutil.hmac_sha256 dr_key credentials.service in
  Cryptoutil.hmac_sha256 drs_key "aws4_request"

let sign_string_v4 ~key credentials stringtosign =
  Cryptoutil.to_hex (Cryptoutil.hmac_sha256 (signing_key_v4 key credentials) stringtosign)

type auth_header =
  | AuthEmpty | AuthNone | AuthMalformed of string | AuthDuplicate
  | Authorization of string * string * float option
  | AuthorizationV4 of auth_v4 * string * float option

let did_expire_v4 date expires =
  if expires = "" then true
  else
    try (date +. Int64.to_float (Int64.of_string expires)) < Unix.gettimeofday ()
    with _ -> true (* consider expired if malformed *)
;;

let split_semicolon = Netstring_str.regexp_string ";"

let set_of_header_list s =
  List.fold_left (fun accum h -> Headers.StringSet.add h accum)
    Headers.StringSet.empty (Netstring_str.split split_semicolon s)

let split_slash = Netstring_str.regexp_string "/"
let parse_credential c =
  match Netstring_str.bounded_split split_slash c 5 with
  | [ keyid; date_ymd; region; service; "aws4_request" ] ->
    { keyid = keyid; date_ymd = date_ymd; region = region; service = service }
  | _ -> failwith ("Cannot parse credential: " ^ c)

let parse_authorization req =
  match Headers.field_values req.headers "authorization" with
  | [] ->
    let v4_algo = get_query_param_opt req.query_params "X-Amz-Algorithm" in
    if v4_algo = "AWS4-HMAC-SHA256" then begin
      let credential = get_query_param_opt req.query_params "X-Amz-Credential"
      and date = Headers.parse_iso8601 (get_query_param_opt req.query_params "X-Amz-Date")
      and expires = get_query_param_opt req.query_params "X-Amz-Expires"
      and signed = get_query_param_opt req.query_params "X-Amz-SignedHeaders"
      and signature = get_query_param_opt req.query_params "X-Amz-Signature"
      in
      let expiration = date +. Int64.to_float (Int64.of_string expires) in
      AuthorizationV4 ({
          signed_headers = set_of_header_list signed;
          credential = parse_credential credential;
          auth_date = date;
          payloadhash = Some "UNSIGNED-PAYLOAD"
        }, signature, Some expiration)
    end else
      let keyid = get_query_param_opt req.query_params "AWSAccessKeyId"
      and signature = get_query_param_opt req.query_params "Signature"
      and expires = req.expires in
      if keyid = "" && signature = "" && expires = "" then
        AuthNone
      else if keyid = "" || signature = "" || expires = "" then
        AuthEmpty
      else
        Authorization (keyid, signature, Some (Int64.to_float (Int64.of_string expires)))
  | auth :: [] ->
    begin try
        Scanf.sscanf auth "AWS4-HMAC-SHA256 Credential=%s@, SignedHeaders=%s@, Signature=%s"
          (fun credential signed_headers signature ->
             AuthorizationV4 ({
                 signed_headers = set_of_header_list signed_headers;
                 credential = parse_credential credential;
                 auth_date = req.date;
                 payloadhash = None
               }, signature, None)
          )
      with
      | Scanf.Scan_failure s | Failure s ->
        begin try
            Scanf.sscanf auth "AWS %s@:%s" (fun a b -> Authorization (a,b, None))
          with | Scanf.Scan_failure s | Failure s ->
            AuthMalformed s
        end
      | End_of_file -> AuthMalformed "too short"
    end
  | _ ->
    AuthDuplicate
;;

let buf = Buffer.create 128
let gen_debug ~canon =
  Buffer.reset buf;
  Buffer.add_string buf (Bucket.to_string canon.bucket);
  Buffer.add_char buf '\x00';
  Buffer.add_string buf canon.path;
  Buffer.add_char buf '\x00';
  (* todo: string-to-sign, auth id, etc. *)
  Buffer.add_string buf (string_of_float (Unix.gettimeofday ()));
  Cryptoutil.base64_encode (Buffer.contents buf);;

let gen_debug2 info =
  Cryptoutil.base64_encode (info.undecoded_url);;

(* query params without the optional signature parameters *)
let actual_query_params r =
  List.fast_sort compare_nameval (
    List.filter (fun (name,_) ->
        not (name = "AWSAccessKeyId" || name = "Signature" || name = "Expires")
      ) (stringmap_all r.query_params)
  );;
