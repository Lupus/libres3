(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2015 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version.    *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA  02110-1301  USA                                                   *)
(*                                                                        *)
(*  As a special exception to the GNU Library General Public License,     *)
(*  you may link, statically or dynamically, a "work that uses the        *)
(*  Library" with a publicly distributed version of the Library to        *)
(*  produce an executable file containing portions of the Library, and    *)
(*  distribute that executable file under terms of your choice, without   *)
(*  any of the additional requirements listed in clause 6 of the GNU      *)
(*  Library General Public License. By "a publicly distributed version    *)
(*  of the Library", we mean either the unmodified Library, or a          *)
(*  modified version of the Library that is distributed under the         *)
(*  conditions defined in clause 3 of the GNU Library General Public      *)
(*  License. This exception does not however invalidate any other         *)
(*  reasons why the executable file might be covered by the GNU Library   *)
(*  General Public License.                                               *)
(**************************************************************************)

open Http
open Neturl
open Lwt
open SXDefaultIO
open EventLog

type cluster_nodelist = {
  mutable nodes: string list;
  uuid: string
}

let nodelist_cache = Caching.cache 128

(* TODO: use token_of_user in make_request and write wrapper without *)
let syntax = {
  Neturl.null_url_syntax with
  url_enable_user = Url_part_required;
  url_enable_scheme = Url_part_required;
  url_enable_host = Url_part_required;
  url_enable_port = Url_part_allowed;
  url_enable_path = Url_part_allowed;
  url_accepts_8bits = false;
  url_is_valid = (fun _ -> true);
  url_enable_relative = true;
}

module AsyncJson = struct
  type json_element = [`Bool of bool | `Float of float | `Null | `String of string]
  type json_container = [`Array | `Object | `Field of string]
  type 'a json_mapper = {
    start: 'a -> json_container -> 'a * 'a json_mapper;
    stop: parent:'a -> children:'a -> json_container -> 'a;
    element: 'a -> json_element -> 'a
  }
  type range = (int * int) * (int * int);;

  exception ParseError of (range * string);;

  type 'a json_state = {
    d: Jsonm.decoder;
    input: unit -> (string * int * int) Lwt.t;
    mutable parents: (json_container * 'a * 'a json_mapper) list;
    mutable state: 'a;
    mutable mapper: 'a json_mapper;
  }

  type decoded = [`End | `Error of Jsonm.error | `Lexeme of Jsonm.lexeme]

  let rec decode s =
    match Jsonm.decode s.d with
    | `Await ->
      s.input () >>= fun (str, pos, len) ->
      Jsonm.Manual.src s.d str pos len;
      decode s
    | #decoded as v ->
      return v
  ;;

  let json_fail s msg =
    let r = Jsonm.decoded_range s.d in
    let (l1, c1), (l2, c2) = r in
    warning "Json parse error: %d:%d-%d:%d:%s" l1 c1 l2 c2 msg;
    fail (ParseError (r, msg))
  ;;
  (* run one fold step *)
  let start s v =
    try
      let state, mapper = s.mapper.start s.state v in
      s.parents <- (v, s.state, s.mapper) :: s.parents;
      s.state <- state;
      s.mapper <- mapper;
      return false
    with Failure msg ->
      json_fail s msg
  ;;

  let stop s parent parent_mapper v rest =
    s.mapper <- parent_mapper;
    s.parents <- rest;
    s.state <- s.mapper.stop ~parent ~children:s.state v;
  ;;

  let stop_field s =
    match s.parents with
    | (`Field _ as f, parent, parent_mapper) :: rest ->
      stop s parent parent_mapper f rest;
      false
    | [] ->
      true (* EOF *)
    | _ ->
      false
  ;;

  let stop_container s v =
    match s.parents with
    | (parent_type, parent, parent_mapper) :: rest->
      if parent_type = v then begin
        try
          stop s parent parent_mapper v rest;
          return (stop_field s)
        with Failure msg ->
          json_fail s msg
      end else
        json_fail s "Array/Object end mismatch"
    | [] ->
      json_fail s "Array/Object end without start"
  ;;

  let fold_step_signal s v =
    match v with
    | `Lexeme `As ->
      start s `Array
    | `Lexeme `Ae ->
      stop_container s `Array;
    | `Lexeme `Os ->
      start s `Object
    | `Lexeme `Oe ->
      stop_container s `Object;
    | `Lexeme (`Name n) ->
      start s (`Field n)
    | `Lexeme (#json_element as v) ->
      begin try
          s.state <- s.mapper.element s.state v;
          return (stop_field s)
        with Failure msg ->
          json_fail s msg
      end
    | `End ->
      begin match s.parents with
        | [] -> return true (* EOF *)
        | _ -> json_fail s "Premature EOF"
      end
    | `Error err ->
      Jsonm.pp_error Format.str_formatter err;
      json_fail s (Format.flush_str_formatter ())
  ;;

  let fold_init input state mapper = {
    d = Jsonm.decoder `Manual;
    input = input;
    parents = [];
    state = state;
    mapper = mapper
  };;

  let fold_step s =
    decode s >>= fold_step_signal s
  ;;

  let rec fold s =
    fold_step s >>= function
    | true -> return s.state
    | false -> fold s
  ;;

  type json = [`A of json list | `O of json list | `F of string * json list |
               json_element]

  open Printf
  let pp_json_lst () lst =
    let buf = Buffer.create 128 in
    let rec pp_json = function
      | `A l ->
        bprintf buf "array (\n";
        List.iter pp_json l;
        bprintf buf ")\n";
      | `O l ->
        bprintf buf "object (\n";
        List.iter pp_json l;
        bprintf buf ")\n";
      | `F (n, l) ->
        bprintf buf "field %s:(\n" n;
        List.iter pp_json l;
        bprintf buf ")";
      | `Bool b ->
        bprintf buf "bool: %b\n" b
      | `String s ->
        bprintf buf "string: %s\n" s
      | `Float f ->
        bprintf buf "number: %g\n" f
      | `Null ->
        bprintf buf "null\n"
    in
    List.iter pp_json lst;
    Buffer.contents buf

  let rec json_parser = {
    start = (fun _ _ -> [], json_parser);
    stop = (fun ~parent ~children v ->
        match v with
        | `Array -> (`A (List.rev children)) :: parent
        | `Object -> (`O (List.rev children)) :: parent
        | `Field name -> (`F (name, children)) :: parent
      );
    element = (fun state v ->
        (v :> json) :: state);
  };;

  let rec read_eof input =
    input () >>= fun (_,_,len) ->
    if len = 0 then return ()
    else read_eof input;;

  let json_parse_tree input =
    fold (fold_init input [] json_parser) >>= fun result ->
    read_eof input >>= fun () ->
    return result
  ;;
end

(* TODO: implement tmpfile buffer here *)
module IO = EventIO
module XIO = SXDefaultIO

module P = Http

(* max upload chunk size, for now we use this as
 * the multipart part-size too *)
let chunk_size = 4 * 1024 * 1024
let download_max_blocks = 30 (* TODO: keep in sync with include/default.h *)
let multipart_threshold = 132 * 1024 * 1024 (* 132 MB, should be multiple of
                                               chunk_size *)
let last_threshold = Int64.of_int chunk_size

let pipe = ref None

let pipeline () =
  match !pipe with
  | None ->
    let p = P.start_pipeline () in
    pipe := Some p;
    p
  | Some p -> p;;

let scheme = "sx"
let syntax = syntax

let rec foldl base volume f lst accum =
  match lst with
  | hd :: tl ->
    f accum {
      XIO.name =
        "/" ^ (Netencoding.Url.encode volume) ^ "/" ^ hd.name;
      XIO.size = hd.size;
      XIO.mtime = hd.mtime;
      XIO.etag = hd.etag;
    } >>= foldl base volume f tl
  | [] ->
    return accum;;


let format_date_header t =
  Netdate.format ~fmt:"%a, %d %b %Y %H:%M:%S GMT" (Netdate.create t)
;;

open Cryptokit
let string_of_method = function
  | `DELETE -> "DELETE"
  | `GET -> "GET"
  | `HEAD -> "HEAD"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | _ -> "N/A";;

let sign_request token r =
  if token = "" then
    failwith "SX token is not set";
  let date = format_date_header (Unix.gettimeofday ()) in
  let headers = ("Date", date) :: r.req_headers in
  let d = transform_string (Base64.decode ()) token in
  let i = String.sub d 0 20
  and k = String.sub d 20 20 in
  let hmac = MAC.hmac_sha1 k in
  let buf = Buffer.create 128 in
  Buffer.add_string buf  (string_of_method r.meth);
  Buffer.add_char buf  '\n';
  let n = String.length r.relative_url in
  let s = String.sub r.relative_url 1 (n-1) in
  Buffer.add_string buf s;
  Buffer.add_char buf  '\n';
  Buffer.add_string buf  date;
  Buffer.add_char buf  '\n';
  let sha1 = hash_string (Hash.sha1 ()) r.req_body in
  let sha1_hex = transform_string (Hexa.encode ()) sha1 in
  Buffer.add_string buf sha1_hex;
  Buffer.add_char buf  '\n';
  let signed = (hash_string hmac (Buffer.contents buf)) in
  let a = i ^ signed ^ (String.sub d 40 2) in
  let auth = "SKY " ^ (transform_string (Base64.encode_compact ()) a) in
  { r with
    req_headers = ("Authorization", auth) :: headers
  }

module Json = AsyncJson
open Json

exception SXProto of string
let expect_content_type reply ct =
  try
    let server = Nethttp.Header.get_server reply.headers in
    let expected = "Skylable" in
    if (String.length server < String.length expected ||
        String.sub server 0 (String.length expected) <> expected) then
      fail (Http_client.Http_protocol
              (SXProto (Printf.sprintf "Not an SX server: %s" server)))
    else
      try
        let actual, _ = reply.headers#content_type () in
        if actual <> ct then
          fail (SXProto (Printf.sprintf
                           "Bad content-type: %s, expected: %s" actual ct))
        else
          return ()
      with Not_found ->
        (* FIXME: don't be so strict if the reply is 304 *)
        fail (SXProto (Printf.sprintf "No Content-Type: header in reply!"))
  with Not_found ->
    fail (SXProto (Printf.sprintf "No Server: header in reply!"))
;;

let http_syntax = Hashtbl.find Neturl.common_url_syntax "http"
let locate url =
  Neturl.modify_url url
    ~encoded:true
    ~query:"o=locate&volumeMeta"
    ~scheme:"http"
    ~syntax:http_syntax;;

let fetch_nodes url =
  Neturl.modify_url url
    ~encoded:true
    ~path:[""]
    ~query:"nodeList&clusterLimits"
    ~scheme:"http"
    ~syntax:http_syntax;;

let url_port_opt url =
  try url_port url
  with Not_found ->
    !Config.sx_port

let delay ms =
  Lwt_unix.sleep (ms /. 1000.)

let request_of_url ~token meth ?(req_body="") ?etag url =
  let syntax = url_syntax_of_url url in
  let relative_url =
    Neturl.remove_from_url
      ~scheme:true ~user:true ~user_param:true ~password:true
      ~host:true ~port:true
      (Neturl.modify_url ~syntax:(Neturl.partial_url_syntax syntax) url) in
  sign_request token {
    meth = meth;
    host = url_host url;
    port = url_port_opt url;
    relative_url = string_of_url relative_url;
    req_headers = [];
    req_body = req_body;
    req_etag = etag;
  }

let filter_field field =
  List.filter (function
      | `F (name,_) when name = field -> true
      | _ -> false
    );;

let has_field field =
  List.exists (function
      | `F (name, _) when name = field -> true
      | _ -> false)
;;

let filter_field_one field lst =
  match filter_field field lst with
  | `F (_,[value]) :: [] ->
    value
  | [] ->
    warning "missing field %s in json: %a" field pp_json_lst lst;
    failwith ("missing field in json: " ^ field)
  | _ ->
    warning "duplicate field in json: %a" pp_json_lst lst;
    failwith "duplicate field in json"
;;

let detail_of_reply reply =
  Lwt.catch (fun () ->
      expect_content_type reply "application/json" >>= fun () ->
      json_parse_tree (P.input_of_async_channel reply.body) >>= function
      | [`O obj] ->
        begin match filter_field "ErrorMessage" obj with
          | `F (_,[`String value]) :: [] ->
            return value
          | _ ->
            return ""
        end
      | _ -> return ""
    )
    (fun e ->
       return ("Unparsable reply" ^ (Printexc.to_string e)));;

let make_http_request ?quick p url =
  P.make_http_request ?quick p url >>= fun reply ->
  let apiverstr =
    try reply.headers#multiple_field "SX-API-Version"
    with Not_found -> ["0"] in
  try
    let apiver = List.fold_left max 0 (List.rev_map int_of_string apiverstr) in
    if apiver < Config.apiver_min || apiver > Config.apiver_max then
      let msg = Printf.sprintf "Unsupported SX API version: %d, expected between %d and %d"
          apiver Config.apiver_min Config.apiver_max in
      fail (XIO.Detail(Http_client.Http_protocol(Failure msg),
                       ["LibreS3ErrorMessage", msg]))
    else return reply
  with Failure _ ->
    let msg = "Invalid SX API version: " ^ (String.concat "," apiverstr) in
    fail (Http_client.Http_protocol(Failure msg))

let usercache = Caching.cache 128
let rec make_request_token ?quick ~token meth ?(req_body="") ?etag url =
  (* TODO: check that scheme is sx! *)
  let p = pipeline () in
  make_http_request ?quick p (request_of_url ~token meth ~req_body ?etag url) >>= fun reply ->
  if reply.status = `Ok || reply.status = `Partial_content || reply.status = `Not_modified then
    return reply
  else if reply.code = 429 then
    (* TODO: next in list, better interval formula *)
    delay 20. >>= fun () ->
    make_request_token ~token meth ~req_body ?etag url
  else
    let url = Neturl.modify_url ~scheme:"http" url in
    detail_of_reply reply >>= fun detail ->
    let code = match reply.status with
      | `Not_found | `Bad_request (* SX bug: returns 400 instead of 404 *) ->
        Some Unix.ENOENT
      | `Unauthorized -> Some Unix.EACCES
      | `Forbidden -> Some Unix.EACCES
      | `Conflict ->
        Some (if meth=`DELETE then Unix.ENOTEMPTY else Unix.EEXIST)
      | `Request_uri_too_long -> Some Unix.ENAMETOOLONG
      | `Requested_range_not_satisfiable -> Some Unix.EINVAL
      | `Request_entity_too_large -> Some Unix.ENOSPC
      | _ -> None in
    let details = [
      "SXErrorMessage",detail;
      "SXHttpCode", string_of_int reply.code
    ] in
    let details = if reply.status = `Unauthorized then ["LibreS3ErrorMessage", detail] else details in
    match code with
    | Some c ->
      fail (XIO.Detail(
          Unix.Unix_error(c,string_of_method meth,string_of_url url),
          details))
    | None ->
      fail (XIO.Detail (
          Failure ((string_of_method meth) ^ " " ^ (string_of_url url)),
          details));;

let filter_field_int field lst =
  match filter_field_one field lst with
  | `Float f ->
    (* TODO: check that it doesn't have fractional part *)
    Int64.of_float f
  | _ ->
    failwith "numeric field expected";;

let filter_field_array field lst =
  match filter_field_one field lst with
  | `A a -> a
  | _ ->
    failwith "array field expected";;

let filter_field_number field lst =
  match filter_field_one field lst with
  | `Float f -> f
  | _ ->
    failwith "numeric field expected";;

let filter_field_string field lst =
  match filter_field_one field lst with
  | `String f -> f
  | _ ->
    failwith "string field expected";;

module AJson = AsyncJson

let token_of_user url =
  let user = Neturl.url_user url in
  if user = !Config.key_id then
    return (Some !Config.secret_access_key)
  else
    let fetch ?etag user =
      let url = Neturl.remove_from_url ~query:true
          (Neturl.modify_url ~path:["";".users";user] url ~scheme:"http") in
      make_request_token ~quick:true ~token:!Config.secret_access_key `GET url in
    let parse reply =
      json_parse_tree (P.input_of_async_channel reply.body) >>= function
      | [`O obj] ->
        let key = filter_field_string "userKey" obj in
        let b64 = Base64.encode_compact () in
        let auth_uid = hash_string (Hash.sha1 ()) user in
        b64#put_string auth_uid;
        b64#put_string (transform_string (Hexa.decode ()) key);
        b64#put_string "\x00\x00";
        b64#finish;
        return (Some b64#get_string)
      | lst ->
        warning "bad user info json: %a" pp_json_lst lst;
        fail (Failure "bad user info json")
    in
    Lwt.catch (fun () ->
        Caching.make_global_cached_request usercache ~fetch ~parse user)
      (function
        | Detail(Unix.Unix_error(Unix.ENOENT, _,_), _) ->
          Lwt.return None
        | e -> Lwt.fail e
      )

let invalidate_token_of_user url =
  let user = Neturl.url_user url in
  EventLog.info (fun () -> Printf.sprintf "Invalidating token for user %S" user);
  Caching.invalidate_cached usercache user

let choose_error = function
  | e :: _ ->
    Lwt.fail e
  | [] ->
    failwith "empty error list"

let rec make_request_loop meth ?quick ?req_body ?etag nodes url errors = match nodes with
  | node :: rest ->
    Lwt.catch (fun () ->
        let url = Neturl.modify_url ~host:node url in
        token_of_user url >>= function
        | Some token ->
          make_request_token ?quick ~token meth ?req_body ?etag url
        | None ->
          (* no such user *)
          let user = Neturl.url_user url in
          Lwt.fail (XIO.Detail(Unix.Unix_error(Unix.EACCES,"", user),[
              "SXErrorMessage",Printf.sprintf "Cannot retrieve token for user %S"
                user
            ]))
      ) (fun e ->
        make_request_loop meth ?quick ?req_body ?etag rest url (e :: errors)
      )
  | [] ->
    choose_error errors

let make_request meth ?quick ?req_body ?etag nodes url =
  Lwt.catch (fun () ->
      make_request_loop meth ?quick ?req_body ?etag nodes url []
    ) (fun _ ->
      delay 20. >>= fun () ->
      make_request_loop meth ?quick ?req_body ?etag nodes url []
    )

module StringSet = Set.Make(String)

let parse_server server =
  try
    Scanf.sscanf server "Skylable/%_s (%s@)" (fun s -> s)
  with e ->
    failwith ("Bad servername " ^ server ^ ":" ^ (Printexc.to_string e))
;;

let parse_sx_cluster server =
  try
    Scanf.sscanf server "%_s (%s@)" (fun s -> s)
  with e ->
    failwith ("Bad servername " ^ server ^ ":" ^ (Printexc.to_string e))
;;


let rot l =
  if l = [] then []
  else List.rev_append (List.rev (List.tl l)) [List.hd l]

let node_addr s = match Ipaddr.of_string_exn s with
  | Ipaddr.V4 _ -> s
  | Ipaddr.V6 v6 -> Printf.sprintf "[%s]" (Ipaddr.V6.to_string v6)

let parse_nodelist headers nodes =
  let nodes = List.rev_map (function
      | `String h -> node_addr h
      | _ -> failwith "bad locate nodes format"
    ) nodes in
  let uuid =
    try parse_sx_cluster (headers#field "SX-Cluster")
    with Not_found -> parse_server (headers#field "Server") in
  return { nodes; uuid }

let parse_nodelist_reply reply =
  json_parse_tree (P.input_of_async_channel reply.body) >>= (function
      | [`O [`F ("nodeList", [`A nodes])]] ->
        parse_nodelist reply.headers nodes
      | lst ->
        warning "bad locate nodes json: %a" pp_json_lst lst;
        failwith "bad locate nodes json");;

let fetch_cluster_nodelist url =
  let fetch ?etag _ = make_request_token ~quick:true ~token:!Config.secret_access_key `GET (fetch_nodes url) in
  Caching.make_global_cached_request nodelist_cache "" ~fetch ~parse:parse_nodelist_reply

let last_nodes = ref []

let periodic url _ =
  let p = pipeline () in
  let send host =
    P.periodic p (Neturl.string_of_url (Neturl.modify_url ~host url))
  in
  List.iter send !last_nodes


let get_cluster_nodelist url =
  fetch_cluster_nodelist url >>= fun l ->
  if !last_nodes = [] && l.nodes <> [] then begin
    let url = Neturl.remove_from_url ~user:true ~query:true (fetch_nodes url) in
    let url = Neturl.modify_url ~port:(url_port_opt url) url in
    ignore (Lwt_engine.on_timer 30. true (periodic url))
  end;
  last_nodes := l.nodes;
  let nodes = l.nodes in
  l.nodes <- rot nodes;
  return (nodes, l.uuid)

let get_vol_nodelist url =
  let fetch ?etag volume =
    let url = Neturl.modify_url url ~path:["";volume] in
    get_cluster_nodelist url >>= fun (cluster_nodes, _) ->
    make_request ~quick:true `GET cluster_nodes (locate url)
  in
  match url_path ~encoded:true url with
  | "" :: volume :: _ ->
    let parse reply =
      json_parse_tree (P.input_of_async_channel reply.body) >>= function
      | [`O [`F ("nodeList", [`A nodes]); `F ("volumeMeta", [`O metalist])]] ->
        if has_field "filterActive" metalist then
          fail (Detail(
              Unix.Unix_error(Unix.EACCES, volume, "Volume uses filters"),
              ["LibreS3ErrorMessage","Cannot access a volume that uses filters"]
            ))
        else
          parse_nodelist reply.headers nodes
      | lst ->
        warning "bad locate nodes json: %a" pp_json_lst lst;
        failwith "bad locate nodes json"
    in
    Caching.make_global_cached_request nodelist_cache volume ~fetch ~parse >>= fun l ->
    let nodes = l.nodes in
    l.nodes <- rot nodes;
    return (nodes, l.uuid)
  | _ ->
    (* cannot download the volume or the root *)
    fail (Unix.Unix_error(Unix.EISDIR,"get",(string_of_url url)));;

let remove_obj = function
  | `O [one] -> one
  | p ->
    warning "obj expected: %a" pp_json_lst [p];
    failwith "Bad json reply format: obj with one field (hash) expected"
;;

module StringMap = Map.Make(String)
let process_batch_reply hashes replybody blocksize map =
  snd (List.fold_left (fun (pos, accum) hash ->
      (pos + blocksize, StringMap.add hash (replybody, pos) accum)
    ) (0,map) hashes)
;;

module NodesMap = Map.Make(struct
    type t = string * StringSet.t
    let compare (ah,at) (bh,bt) =
      match String.compare ah bh with
      | 0 -> StringSet.compare at bt
      | n -> n
  end)

let nodelist (hd, tl) = hd :: (StringSet.elements tl)

let map_host = function
  | `String host -> node_addr host
  | _ -> failwith "Bad json node format"

let map_unique_nodes = function
  | [] -> failwith "Hash with no nodes"
  | hd :: nodes ->
    map_host hd, List.fold_left (fun accum j ->
        StringSet.add (map_host j) accum) StringSet.empty nodes
;;

let add_to_nodemap key value map =
  try
    NodesMap.add key (value :: (NodesMap.find key map)) map
  with Not_found ->
    NodesMap.add key [value] map
;;

let group_by_node hashes =
  List.fold_left
    (fun accum -> function
       | `F (hash, [`A nodes]) ->
         add_to_nodemap (map_unique_nodes nodes) hash accum
       | _ -> failwith "Bad json hash format"
    ) NodesMap.empty hashes
;;

let rec split_at_threshold bs limit listin listout current amount =
  match listin with
  | [] ->
    (* the actual order of hashes inside each batch is reversed,
     * but fold_upload reverses it again so upload
     * will happen in ascending offset order to optimize FS reads *)
    List.rev_append listout [current]
  | hd :: tail ->
    let next = amount + bs in
    if (next <= limit) then
      split_at_threshold bs limit tail listout (hd :: current) next
    else
      split_at_threshold bs limit tail (current :: listout) [hd] bs
;;


(* download a part of the file fully into memory.
 * this is needed because we need to send data in stream order,
 * but we have to download them in different order to use batching and
 * multiple hosts *)
let download_full_mem url blocksize hashes =
  let grouped = group_by_node hashes in
  (* launch all requests *)
  let batches = NodesMap.fold (fun nodes hashes accum ->
      let split = split_at_threshold 1 download_max_blocks (List.rev hashes) [] [] 0 in
      List.fold_left (fun accum hashesr ->
          let hashes = List.rev hashesr in
          let batch = (String.concat "" hashes) in
          let u = Neturl.modify_url url
              ~path:["";".data";string_of_int blocksize;batch] in
          (hashes, make_request `GET (nodelist nodes) u) :: accum
        ) accum split
    ) grouped [] in
  (* build map of replys *)
  List.fold_left (fun accum (hashes, request) ->
      accum >>= fun map ->
      (request >>= fun reply ->
       return (process_batch_reply hashes reply.body blocksize map))
    ) (return StringMap.empty) batches >>= fun hashmap ->
  let batch = String.make (blocksize * (List.length hashes)) 'X' in
  let _ = List.fold_left (fun pos hf ->
      match hf with
      | `F (hash, _ ) ->
        let str, strpos = StringMap.find hash hashmap in
        String.blit str strpos batch pos blocksize;
        pos + blocksize
      | _ -> failwith "bad json hash format"
    ) 0 hashes in
  return batch
;;

let meta_cache = Caching.cache 1024

let get_meta url =
  let fetch ?etag _ =
    get_vol_nodelist url >>= fun (nodes, _) ->
    let url = Neturl.modify_url ~syntax:http_syntax url ~query:"fileMeta" in
    make_request ~quick:true `GET nodes ?etag url
  in
  let parse reply =
    expect_content_type reply "application/json" >>= fun () ->
    let input = P.input_of_async_channel reply.body in
    AJson.json_parse_tree input >>= function
    | [`O [`F ("fileMeta", [`O metalist])]] ->
      return (List.rev_map (function
          | `F (k, [`String v]) -> k, transform_string (Hexa.decode()) v
          | _ -> failwith "bad meta reply format"
        ) metalist)
    | _ -> failwith "bad meta reply format"
  in
  Caching.make_private_cached_request meta_cache ~fetch ~parse url

let etag_of_revision = function
  | `String rev ->
    (* could use a hash and a suffix *)
    rev
  | _ ->
    failwith "bad revision format"

type state = {
  hashes: AJson.json list;
  blocksize: int;
  filesize: int64;
  url: Neturl.url;
}
type read_state = unit -> (string * int * int) Lwt.t

let get url =
  get_vol_nodelist url >>= fun (nodes, _) ->
  Lwt.catch (fun () ->
      make_request `GET nodes url >>= fun reply ->
      try
        let mtime = Nethttp.Header.get_last_modified reply.headers in
        expect_content_type reply "application/json" >>= fun () ->
        let input = P.input_of_async_channel reply.body in
        AJson.json_parse_tree input >>= function
        | [`O obj] ->
          let filesize = filter_field_int "fileSize" obj
          and etag = etag_of_revision (filter_field_one "fileRevision" obj)
          and blocksize = Int64.to_int (filter_field_int "blockSize" obj) in
          let a = filter_field_array "fileData" obj in
          let hashes = List.rev (List.rev_map remove_obj a) in
          (* split after DOWNLOAD_MAX_BLOCKS hashes *)
          return ({ hashes = hashes; blocksize = blocksize;
                    filesize = filesize; url = url;},
                  { name = ""; size = filesize;
                    mtime = mtime; etag = etag;
                  })
        | p ->
          warning "bad json reply format: object expected at %a" pp_json_lst p;
          fail (Failure "Bad json reply format: object expected")
      with Not_found ->
        fail (Failure ("Last-Modified missing in SX reply: "
                       ^ (string_of_url  url)))
    )
    (function
      | Detail(Unix.Unix_error(Unix.ENOENT,_,_) as e ,_) ->
        fail e
      | e -> fail e);;


let read (_, reader) = reader ()

(* drop hashes from start of list until we reach desired position *)
let rec seek_hashes bs target_pos hashes pos = match hashes with
  | _ :: tl as l ->
    let next = Int64.add pos bs in
    if next > target_pos then l, pos
    else seek_hashes bs target_pos tl next
  | [] -> [], pos

let download_hashes (push, url, blocksize, remaining, skip) hashes_rev =
  let n = List.length hashes_rev in
  let offset = skip in
  let reply = download_full_mem url blocksize (List.rev hashes_rev) in
  let len = min (Int64.mul (Int64.of_int n) (Int64.of_int blocksize)) remaining in
  let remaining = Int64.sub remaining len in
  push#push (reply >|= fun s -> (s, offset, Int64.to_int len - offset)) >|= fun () ->
  (push, url, blocksize, remaining, 0)

let seek s pos =
  let hashes, start = seek_hashes (Int64.of_int s.blocksize) pos s.hashes 0L in
  let nodes = max 1 (List.length !last_nodes) in
  let split_map = split_at_threshold s.blocksize
      (s.blocksize * nodes * download_max_blocks) hashes [] [] 0 in
  let remaining = Int64.sub s.filesize start in
  let skip = Int64.to_int (Int64.sub pos start) in
  let download, download_push = Lwt_stream.create_bounded 4 in
  Lwt.ignore_result (Lwt.try_bind (fun () ->
      Lwt_list.fold_left_s download_hashes (download_push, s.url, s.blocksize, remaining, skip) split_map)
      (fun _ -> download_push#close; return_unit)
      (fun exn -> download_push#push (fail exn) >>= fun () -> download_push#close; return_unit));
  return (s, fun () ->
      Lwt_stream.get download >>= function
      | None -> return ("", 0, 0)
      | Some r -> r
    )

let remove_leading_slash name =
  let n = String.length name in
  if n > 0 && name.[0] = '/' then
    String.sub name 1 (n-1)
  else
    name

let parse_file = function
  | `F (name,[`O meta]) ->
    {
      name = remove_leading_slash name;
      size = (try (match filter_field_one "fileSize" meta with
          | `Float f -> Int64.of_float f
          | _ -> failwith "bad filesize format") with _ -> 0L);
      mtime = (try (match filter_field_one "createdAt" meta with
          | `Float f -> f
          | _ -> failwith "bad mtime format") with _ -> 0.);
      etag = try etag_of_revision (filter_field_one "fileRevision" meta) with _ -> "";
    }
  | p ->
    warning "bad filelist format: %a" pp_json_lst [p];
    failwith "bad filelist format"
;;

(* SX is too slow when listing directories on large volumes,
 * cache the data until bug #415 is fixed *)
(* TODO: pendinglist *)
module ListCache = Pendinglimit.Make(Lwt)(struct
    type t = string * string
    let compare = Pervasives.compare
  end)
let listcache = ListCache.create ()

let listit url =
  let user = Neturl.url_user url in
  let req = user, string_of_url url in
  ListCache.bind listcache req (fun _ ->
      get_vol_nodelist url >>= fun (nodes, _) ->
      make_request `GET nodes url >>=  fun reply ->
      begin
        expect_content_type reply "application/json" >>= fun () ->
        let input = P.input_of_async_channel reply.body in
        json_parse_tree input >>= function
        | [`O obj] ->
          (* TODO: stream parse instead *)
          begin match filter_field_one "fileList" obj with
            | `O files ->
              return (List.rev_map parse_file files)
            | p ->
              warning "bad volume list format: %a" pp_json_lst [p];
              fail (Failure "bad volume list format")
          end
        | p ->
          warning "bad volume list format: %a" pp_json_lst p;
          fail (Failure "bad volume list format2")
      end
    )

let parse_volume = function
  | `F (name,[`O _meta]) -> name
  | p ->
    warning "bad volumes list format: %a" pp_json_lst [p];
    failwith "bad volumeslist format"
;;

let filter_opt l =
  List.fold_left (fun accum -> function
      | Some e -> e :: accum | None -> accum) [] l

let remove_volumes_filters url l =
  Lwt_list.rev_map_p (fun vol ->
      let url = Neturl.modify_url url ~path:[""; vol] in
      Lwt.catch (fun () ->
          get_vol_nodelist url >>= fun _ -> return (Some vol))
        (fun _ -> return None)) l >>= fun lst ->
  return (filter_opt lst)

let volumelist url =
  get_cluster_nodelist url >>= fun (cluster_nodes, _) ->
  make_request `GET cluster_nodes url >>=  fun reply ->
  begin
    expect_content_type reply "application/json" >>= fun () ->
    let input = P.input_of_async_channel reply.body in
    json_parse_tree input >>= function
    | [`O obj] ->
      begin match filter_field_one "volumeList" obj with
        | `O files ->
          remove_volumes_filters url (List.rev_map parse_volume files)
        | p ->
          warning "bad volumes list format: %a" pp_json_lst [p];
          fail (Failure "bad volumes list format")
      end
    | p ->
      warning "bad volumes list format: %a" pp_json_lst p;
      fail (Failure "bad volumes list format2")
  end


let rec print_json e = function
  | `Array a ->
    ignore (Jsonm.encode e (`Lexeme `As));
    List.iter (print_json e) a;
    ignore (Jsonm.encode e (`Lexeme `Ae));
  | `Object o ->
    ignore (Jsonm.encode e (`Lexeme `Os));
    List.iter (print_json_mem e) o;
    ignore (Jsonm.encode e (`Lexeme `Oe));
  | `Null | `Bool  _ | `Float _ | `String _ as l ->
    ignore (Jsonm.encode e (`Lexeme l))

and print_json_mem e (n, v) =
  ignore (Jsonm.encode e (`Lexeme (`Name n)));
  print_json e v;;

let send_json nodelist url json =
  let b = Buffer.create 4096 in
  let encoder = Jsonm.encoder (`Buffer b) in
  print_json encoder json;
  ignore (Jsonm.encode encoder `End);
  make_request `PUT ~req_body:(Buffer.contents b) nodelist url;;

type estate = EOF | PartialBlock of int | FullBlock

type buf = {
  buf: string;
  mutable str: string;
  mutable pos: int;
  mutable n: int
}

let rec read_block stream buf pos size =
  if size > 0 then
    let read = if buf.pos + buf.n <= String.length buf.str && buf.n > 0 then
        return ()
      else begin
        stream () >>= fun (src, srcpos, n) ->
        buf.str <- src;
        buf.pos <- srcpos;
        buf.n <- n;
        return ()
      end in
    read >>= fun () ->
    let n = min buf.n size in
    String.blit buf.str buf.pos buf.buf pos n;
    buf.pos <- buf.pos + n;
    buf.n <- buf.n - n;
    if n = 0 then
      if pos = 0 then
        return EOF
      else
        return (PartialBlock pos)
    else
      read_block stream buf (pos + n) (size - n)
  else
    return FullBlock
;;

(* TODO: these would belong in eventIO *)
let rec compute_hashes_loop uuid tmpfd stream buf blocksize lst map pos stop =
  if pos >= stop then return (lst, map, stop)
  else begin
    String.fill buf.buf 0 (String.length buf.buf) '\x00';
    read_block stream buf 0 blocksize >>= fun status ->
    IO.really_write tmpfd buf.buf 0 (String.length buf.buf) >>= fun () ->
    if status = EOF then return (lst, map, pos)
    else
      let h = Hash.sha1 () in
      h#add_string uuid;
      let sha1 = hash_string h buf.buf in
      let sha1_hex = transform_string (Hexa.encode ()) sha1 in
      let nextmap = StringMap.add sha1_hex pos map in
      let nextlst = sha1_hex :: lst in
      let nextpos = Int64.add pos (Int64.of_int blocksize) in
      match status with
      | FullBlock ->
        compute_hashes_loop uuid tmpfd stream buf blocksize nextlst nextmap nextpos stop
      | PartialBlock len ->
        return (nextlst, nextmap, Int64.add pos (Int64.of_int len)) (* it was a partial block, we're done *)
      | _ -> assert false
  end
;;

let fold_upload user port (offset, tmpfd) map token blocksize nodes hashes previous =
  previous >>= fun () ->
  let buf = {
    buf = String.make (blocksize * (List.length hashes)) '\x00';
    str = ""; pos = 0; n = 0
  } in
  (*  TODO: chop into 4MB chunks *)
  List.fold_left (fun prev hash ->
      prev >>= fun pos ->
      try
        buf.pos <- 0;
        buf.n <- 0;
        let seekpos = StringMap.find hash map in
        (*          Printf.printf "Uploading hash %s from offset %Ld\n" hash seekpos;*)
        let offset = Int64.sub seekpos offset in
        IO.lseek tmpfd offset >>= fun _ ->
        IO.really_read tmpfd buf.buf pos blocksize >>= function
        | 0 -> fail (Failure "eof when trying to read hash")
        | _ ->
          return (pos + blocksize)
      with Not_found ->
        debug (fun () ->
            let buf = Buffer.create 128 in
            Buffer.contents (StringMap.fold (fun k v buf -> Printf.bprintf buf "%s -> %Ld\n" k v; buf) map buf)
          );
        fail (Failure ("hash not found:" ^ hash))
    ) (return 0) hashes >>= fun _ ->
  (* TODO: retry on failure *)
  let url =
    Neturl.make_url ~encoded:false
      ~scheme:"sx"
      ~port ~path:["";".data";string_of_int blocksize;token]
      ~user ~host:(fst nodes)
      http_syntax
  in
  make_request `PUT ~req_body:buf.buf (nodelist nodes) url >>= fun _ ->
  return ()
;;

let check_reply reply =
  let c = reply.code in
  if c >= 400 then
    fail (Failure (Printf.sprintf "SX replied with code %d" c))
  else
    return ()
;;

let rec job_poll origurl url expected_id interval max_interval =
  delay interval >>= fun () ->
  make_request `GET [Neturl.url_host url] url >>= fun reply ->
  AJson.json_parse_tree (P.input_of_async_channel reply.body) >>= function
  | [`O obj] ->
    let requestid = filter_field_string "requestId" obj
    and status = filter_field_string "requestStatus" obj
    and msg = filter_field_string "requestMessage" obj in
    if requestid <> expected_id then
      fail (XIO.Detail(
          Failure "Job id mismatch",
          ["SXErrorMessage",msg;"SXJobStatus",status;
           "ExpectedId",expected_id;"ActualId",requestid]))
    else begin match status with
      | "PENDING" ->
        (* TODO: investigate other formulas, for now we use libsx formula *)
        let interval = min (interval *. 2.0) max_interval in
        job_poll origurl url expected_id interval max_interval
      | "OK" ->
        return ()
      | "ERROR" ->
        let e = match msg with
          | "Volume already exists" | "File exists" ->
            Unix.Unix_error(Unix.EEXIST,"PUT",string_of_url origurl)
          | _ ->
            Failure ("Operation failed: " ^ msg) in
        fail (XIO.Detail(e, ["SXErrorMessage", msg;"SXHttpCode","200"]))
      | _ ->
        fail (XIO.Detail(
            Failure (Printf.sprintf "Invalid request status %s: %s" status msg),
            ["SXErrorMessage", msg;"SXHttpCode","200"]))
    end
  | p ->
    warning "bad json reply (object expected): %a" pp_json_lst p;
    fail (Failure "Bad json reply format: object expected")
;;

let job_get ?(async=false) url reply =
  check_reply reply >>= fun () ->
  if async then return ()
  else AJson.json_parse_tree (P.input_of_async_channel reply.body) >>= function
    | [`O obj] ->
      let requestid = filter_field_string "requestId" obj
      and minPoll = filter_field_number "minPollInterval" obj
      and maxPoll = filter_field_number "maxPollInterval" obj in
      let pollurl = Neturl.modify_url
          ~path:["";".results";requestid]
          (Neturl.modify_url ~host:reply.req_host url) in
      job_poll url pollurl requestid minPoll maxPoll
    | p ->
      warning "bad json reply (object expected): %a" pp_json_lst p;
      fail (Failure "Bad json reply format: object expected")
;;

let flush_token url token =
  let url = Neturl.modify_url url
      ~path:["";".upload";token] ~encoded:false in
  make_request `PUT [Neturl.url_host url] url >>=
  job_get url

let locate_upload url size =
  match url_path ~encoded:true url with
  | "" :: volume :: _ ->
    get_vol_nodelist url >>= fun (nodes, _) ->
    begin make_request `GET nodes (Neturl.modify_url
                                     ~path:["";volume]
                                     ~scheme:"http"
                                     ~syntax:http_syntax
                                     ~encoded:true
                                     ~query:(Printf.sprintf "o=locate&volumeMeta&size=%Ld" size) url)
      >>= fun reply ->
      AJson.json_parse_tree (P.input_of_async_channel reply.body) >>= function
      | [`O [
          `F ("nodeList",[`A nodes]);
          `F ("blockSize",[`Float blocksize]);
          `F ("volumeMeta", [`O metalist])
        ]] ->
        if has_field "filterActive" metalist then
          fail (XIO.Detail(
              Unix.Unix_error(Unix.EACCES, volume, "Volume uses filters"),
              ["LibreS3ErrorMessage","Cannot access a volume that uses filters"]
            ))
        else let nodes = List.rev_map map_host nodes in
          let uuid =
            try parse_sx_cluster (reply.headers#field "SX-Cluster")
            with Not_found -> parse_server (reply.headers#field "Server")
          in
          return (uuid, nodes, int_of_float blocksize)
      | p ->
        warning "bad json locate format: %a" pp_json_lst p;
        fail (Failure "bad json locate format")
    end
  | _ -> fail (Invalid_argument "Can upload only to a file (not a volume or the root)")
;;

let upload_batch user port source map token blocksize upload_map () =
  let grouped_hashes = group_by_node upload_map in
  NodesMap.fold
    (fold_upload user port source map token blocksize)
    grouped_hashes (return ())
;;

let build_meta = function
  | None -> []
  | Some f ->
    ["fileMeta", `Object (
        List.rev_map (fun (k,v) ->
            k, `String (transform_string (Hexa.encode ()) v)
          ) (f ())
      )]

let upload_part ?metafn nodelist url source blocksize hashes map size extendSeq token =
  let obj = List.rev_append [
      "fileData", `Array hashes;
      match token with
      | Some _ ->
        "extendSeq", `Float (Int64.to_float extendSeq)
      | None ->
        "fileSize", `Float (Int64.to_float size);
    ] (build_meta metafn) in
  send_json nodelist url (`Object obj) >>= fun reply ->
  check_reply reply >>= fun () ->
  AJson.json_parse_tree (P.input_of_async_channel reply.body) >>= function
  | [`O [
      `F ("uploadToken",[`String token]);
      `F ("uploadData",[`O upload_map])
    ]] ->
    debug (fun () -> Printf.sprintf "offering %d hashes, requested %d hashes\n%!"
              (List.length hashes) (List.length upload_map));
    let split_map = split_at_threshold blocksize chunk_size upload_map [] [] 0 in
    List.fold_left
      (fun accum umap ->
         let user = Neturl.url_user url in
         accum >>= upload_batch user (url_port_opt url) source map token blocksize umap)
      (return ()) split_map >>= fun () ->
    return (reply.req_host, token)
  | p ->
    warning "bad json reply format: %a" pp_json_lst p;
    fail (Failure "Bad json reply format")
;;

(* !! partial blocks can only be sent in the final upload request,
 * because otherwise auto-bs would choose the wrong bs
 * when uploading the partial blocks.
 * partial blocks = stuff under auto-bs threshold
*)
let rec upload_chunks ?metafn buf tmpfd nodes url size uuid stream blocksize pos token =
  let endpos = min (Int64.add pos (Int64.of_int multipart_threshold)) size in
  let end_threshold = Int64.sub size last_threshold in
  let endpos =
    (* upload the very last chunk of the file separately:
     * first time: pos < end_threshold -> end_threshold
     * second time: pos = end_threshold -> endpos *)
    if end_threshold > 0L && pos < end_threshold then end_threshold
    else endpos in
  let bs64 = Int64.of_int blocksize in
  let extendseq = Int64.div (Int64.add pos (Int64.sub bs64 1L)) bs64 in
  match token with
  | Some token when pos = endpos ->
    (* all multiparts uploaded, flush token *)
    flush_token url token
  | _ ->
    (* still have parts to upload *)
    compute_hashes_loop uuid tmpfd stream buf blocksize [] StringMap.empty pos endpos
    >>= fun (hashes_rev, map, _) ->
    let hashes = List.rev_map (fun h -> `String h) hashes_rev in
    let n = List.length hashes_rev in
    (* TODO: multiple hosts and retry *)
    let expected_bytes = Int64.to_int (Int64.sub endpos pos) in
    let expected = (expected_bytes + blocksize - 1) / blocksize in
    if (n <> expected) then
      fail (Failure (Printf.sprintf "Bad hash counts: %d != %d; pos: %Ld, bytes: %d"
                       n expected pos expected_bytes))
    else
      let url =
        match token with
        | Some token ->
          Neturl.modify_url url ~path:["";".upload";token] ~encoded:false
        | None -> url in
      IO.lseek tmpfd 0L >>= fun _ ->
      let metafn_final = if endpos = size then metafn else None in
      upload_part ?metafn:metafn_final nodes url (pos, tmpfd) blocksize hashes map size extendseq token >>= fun (host, token) ->
      let url = Neturl.modify_url ~host url in
      IO.lseek tmpfd 0L >>= fun _ ->
      upload_chunks ?metafn buf tmpfd [host] url size uuid stream blocksize endpos (Some token)
;;

let put ?quotaok ?metafn src srcpos url =
  let host = url_host url in
  let size = Int64.sub (src.XIO.meta.XIO.size)  srcpos in
  if size < 0L then
    fail (Failure "Source position beyond EOF")
  else match url_path ~encoded:true url with
    | "" :: _volume :: _path ->
      (* TODO: handle no such volume errors *)
      locate_upload url size >>= fun (uuid, nodes, blocksize) ->
      let url = Neturl.modify_url ~host url in
      src.XIO.seek 0L >>= fun stream ->
      IO.with_tempfile (fun tmpfd ->
          let buf = {
            buf = String.make blocksize '\x00';
            str = ""; pos = 0; n = 0 } in
          begin match quotaok with
            | Some fn ->
              upload_part ?metafn nodes url (0L, tmpfd) blocksize [] StringMap.empty size 0L None >|= fun (host, token) ->
              fn ();
              Neturl.modify_url ~host url, Some token
            | None -> return (url, None)
          end >>= fun (url, token) ->
          upload_chunks ?metafn buf tmpfd nodes url size uuid stream blocksize srcpos token)
    | _ ->
      fail (Failure "can only put a file (not a volume or the root)")

let fold_list url ?marker ?limit ?(no_recurse=false) f recurse accum =
  let fullpath = url_path ~encoded:true url in
  match fullpath with
  | "" :: volume :: path ->
    let base = Neturl.modify_url url ~encoded:true ~path:["";volume] in
    let recursive =
      match no_recurse, limit with
      | true, _ -> ""
      | false, Some limit -> Printf.sprintf "recursive&limit=%d" limit
      | false, None -> "recursive" in
    let query = match path with
      | [] | [""] -> recursive
      | _ ->
        Printf.sprintf "%s&filter=%s" recursive
          ((join_path path) ^ "*") in
    let query = match marker with
      | Some m ->
        Printf.sprintf "%s&after=%s" query (Netencoding.Url.encode ~plus:false m)
      | None -> query in
    let url = Neturl.modify_url url
        ~encoded:true ~scheme:"http" ~syntax:http_syntax
        ~path:["";volume] ~query in
    listit url >>= fun lst ->
    foldl base volume f lst accum
  | [""] | [] ->
    let base = Neturl.modify_url url
        ~encoded:true ~path:[""] ~scheme:"http" ~syntax:http_syntax
        ~query:"volumeList" in
    volumelist base >>= fun lst ->
    List.iter (fun dir ->
        ignore (recurse ("/" ^ dir))) lst;
    return accum
  | _ ->
    failwith "invalid URL";;

let open_source url =
  get url >|= fun (reader, entry) ->
  {
    XIO.name = Neturl.join_path (Neturl.url_path ~encoded:false url);
    size = entry.size;
    mtime = entry.mtime;
    etag = entry.etag
  },
  reader

let close_source _ =
  (* TODO: abort download*)
  return ()

let extract_hash = function
  | `O [ `F (hash, _) ] -> `String hash
  | p ->
    warning "bad json hashlist: %a" pp_json_lst [p];
    failwith "bad json hashlist format"

let extract_hashes lst = List.rev (List.rev_map extract_hash lst)

let get_hashlist src =
  get_vol_nodelist src >>= fun (src_nodes, _) ->
  make_request `GET src_nodes src >>= fun reply ->
  expect_content_type reply "application/json" >>= fun () ->
  json_parse_tree (P.input_of_async_channel reply.body) >>= function
  | [`O obj] ->
    return (filter_field_int "blockSize" obj,
            filter_field_int "fileSize" obj,
            extract_hashes (filter_field_array "fileData" obj))
  | _ ->
    failwith "bad json hash list"

let is_file_url url = match url_path ~encoded:true url with
  | "" :: _dstvol :: _dstpath -> true
  | _ -> false

let upload_hashes ?metafn hashes dst_nodes dst size =
  Lwt.catch (fun () ->
      get_vol_nodelist dst >>= fun (dst_nodes, _) ->
      let obj =
        ("fileSize", `Float (Int64.to_float size)) ::
        ("fileData", `Array hashes) ::
        (build_meta metafn) in
      let obj = List.rev_append (build_meta metafn) obj in
      send_json dst_nodes dst (`Object obj) >>= fun reply ->
      check_reply reply >>= fun () ->
      begin AJson.json_parse_tree (P.input_of_async_channel reply.body) >>= function
        | [`O [
            `F ("uploadToken",[`String token]);
            `F ("uploadData",[`O []])
          ]] ->
          let dst = Neturl.modify_url ~host:reply.req_host dst in
          flush_token dst token >>= fun () ->
          return true
        | _ ->
          return false
      end
    ) (function
      | XIO.Detail(Unix.Unix_error(Unix.ENOENT,_,_) as e ,_) ->
        fail e
      | e -> fail e)

let copy_same ?metafn ?filesize urls dst =
  if not (is_file_url dst) then return false
  else match filesize, urls with
    | Some size, (first :: rest) when List.for_all is_file_url urls ->
      locate_upload dst size >>= fun (_, dst_nodes, blocksize) ->
      let bsize64 = Int64.of_int blocksize in
      if Int64.div size bsize64 > 100000L then
        return false
      else begin
        get_hashlist first >>= fun (first_blocksize, first_size, first_hashes) ->
        if first_blocksize <> bsize64 then
          return false
        else
          Lwt_list.rev_map_p get_hashlist rest >>= fun lst ->
          match List.fold_left (fun (ok, sum_size, rev_hashes) (src_blocksize, src_size, src_hashes) ->
              if src_blocksize <> bsize64 || Int64.rem sum_size bsize64 <> 0L then
                false, 0L, []
              else
                ok, Int64.add sum_size src_size, List.rev_append src_hashes rev_hashes
            ) (true, first_size, List.rev first_hashes) (List.rev lst) with
          | false, _, _ -> return false
          | true, sum_size, rev_hashes ->
            if sum_size <> size then begin
              debug (fun () -> Printf.sprintf "copy_same size mismatch: %Ld <> %Ld\n%!" sum_size size);
              return false
            end else
              upload_hashes ?metafn (List.rev rev_hashes) dst_nodes dst size
      end
    | None, [ single_src ] when is_file_url single_src ->
      get_hashlist single_src >>= fun (_, size, hashes) ->
      get_vol_nodelist dst >>= fun (dst_nodes, _) ->
      upload_hashes ?metafn hashes dst_nodes dst size
    | _ -> return false

let exists url =
  let p = pipeline () in
  token_of_user url >>= function
  | None ->
    return false
  | Some token ->
    begin match url_path ~encoded:true url with
      | "" :: _volume :: ("" :: [] | []) ->
        (* does volume exist? *)
        make_http_request ~quick:true p (request_of_url ~token `HEAD (locate url))
        >>= fun reply ->
        return (reply.status = `Ok)
      | "" :: _volume :: _path ->
        Lwt.catch (fun () ->
            get_vol_nodelist url >>= fun (nodes, _) ->
            make_request ~quick:true `HEAD nodes url >>= fun reply ->
            expect_content_type reply "application/json" >>= fun () ->
            return true
          ) (function
            | XIO.Detail(Unix.Unix_error(Unix.ENOENT, _,_), _) ->
              return false
            | e -> fail e)
      | _ ->
        (* can I fetch the nodeslist? *)
        make_http_request ~quick:true p (request_of_url ~token `HEAD (fetch_nodes url))
        >>= fun reply ->
        return (reply.status = `Ok)
    end

let check url =
  Lwt.catch (fun () ->
      get_cluster_nodelist url >>= fun (_, uuid) ->
      return (Some uuid)
    ) (function
      | XIO.Detail(e, details) ->
        let msg = try List.assoc "SXErrorMessage" details with Not_found -> "" in
        fail (Failure (Printf.sprintf
                         "Remote SX server reports: %s (%s)" msg (Printexc.to_string e))
             )
      | e -> fail e)

let map_perm = function
  | `String "read" -> `Read
  | `String "write" -> `Write
  | `String "owner" -> `Owner
  | `String "manager" -> `Manager
  | p ->
    warning "bad ACL json: %a" pp_json_lst [p];
    failwith "bad ACL json"

let map_acl = function
  | `F (name, [`A perms]) ->
    `Grant, `UserName name, List.rev_map map_perm perms
  | (p:json) ->
    warning "bad ACL json: %a" pp_json_lst [p];
    failwith "bad ACL json"

let acl_url =
  Neturl.modify_url ~encoded:true ~query:"o=acl" ~scheme:"http"
    ~syntax:http_syntax

let acl_cache = Caching.cache 128

let get_acl url : acl list Lwt.t =
  let fetch ?etag _ =
    (* cluster_nodelist would suffice, but vol_nodelist gives better error msg
       (404 instead of 500) *)
    get_vol_nodelist url >>= fun (nodes, _) ->
    let url = acl_url url in
    make_request ~quick:true `GET ?etag nodes url
  in
  let parse reply =
    expect_content_type reply "application/json" >>= fun () ->
    let input = P.input_of_async_channel reply.body in
    json_parse_tree input >>= function
    | [`O obj] ->
      return (List.rev_map map_acl obj)
    | p ->
      warning "bad ACL format: %a" pp_json_lst p;
      fail (Failure "bad ACL format")
  in
  Caching.make_private_cached_request acl_cache ~fetch ~parse url

let find_acl_op dir op l =
  List.find_all (fun (gr, id, acl) ->
      gr = dir && List.mem op acl
    ) l

let map_string (_, `UserName id,_)  = `String id

let acl_op key l other = match l with
  | [] -> other
  | l -> (key, `Array (List.rev_map map_string l)) :: other

let json_of_acl set =
  acl_op "grant-read" (find_acl_op `Grant `Read set) (
    acl_op "grant-write" (find_acl_op `Grant `Write set) (
      acl_op "revoke-read" (find_acl_op `Revoke `Read set) (
        acl_op "revoke-write" (find_acl_op `Revoke `Write set) [])))

let set_acl url acls =
  get_vol_nodelist url >>= fun (cluster_nodes, _) ->
  let url = acl_url url in
  let json = json_of_acl acls in
  send_json cluster_nodes url (`Object json) >>=
  job_get url

let create_user url name =
  get_cluster_nodelist url >>= fun (cluster_nodes, _) ->
  let url = Neturl.modify_url ~encoded:true ~path:[""; ".users"] ~scheme:"http"
      ~syntax:http_syntax ~user:!Config.key_id url in
  Lwt.catch (fun () ->
      make_request `GET cluster_nodes
        (Neturl.modify_url ~path:["";".users";name] url) >>= fun _ ->
      return ""
    ) (function
      | XIO.Detail (e, details) ->
        let key = Cryptokit.transform_string (Cryptokit.Hexa.encode ())
            (Random.string IO.rng 20) in
        let json = [
          "userName", `String name;
          "userType", `String "normal";
          "userKey", `String key
        ] in
        send_json cluster_nodes url (`Object json) >>=
        job_get url >>= fun () ->
        return key
      | e -> fail e
    )

let create ?metafn ?(replica=(!Config.replica_count)) url =
  match url_path url ~encoded:true with
  | "" :: volume :: ([""] | []) ->
    (* elevate to admin privileges for volume creation *)
    let owner = Neturl.url_user url in
    let url =
      if !Config.volume_create_elevate_to_admin then
        Neturl.modify_url
          ~path:["";volume] ~user:!Config.key_id url
      else url in
    get_cluster_nodelist url >>= fun (nodes, _) ->
    Lwt.catch
      (fun () ->
         send_json nodes url (`Object [
             (* max size allowed in json is 2^53, in SX it is 2^50*)
             "volumeSize", `Float !Config.volume_size;
             "owner", `String owner;
             "replicaCount", `Float (float_of_int replica)
           ]) >>=
         job_get url
      )
      (function
        | XIO.Detail (Unix.Unix_error(Unix.EEXIST,_,_) as e, _) ->
          fail e
        | e -> fail e
      )
  | path ->
    let rec last l = match l with
      | [] -> invalid_arg "Cannot create the root"
      | _ :: [tl] -> tl
      | _ :: rest -> last rest in
    let path =
      if (last path) = "" then
        path @ ["."] (* can't create files with a trailing slash *)
      else path in
    let url = Neturl.modify_url ~encoded:true ~path url in
    let `Source src = XIO.of_string "" in
    put ?metafn src 0L url;;

let is_owner_of_vol nodes volume_url =
  let owner = Neturl.url_user volume_url in
  let acl_url = Neturl.modify_url ~query:"o=acl" ~scheme:"http"
      ~syntax:http_syntax volume_url in
  make_request `GET nodes acl_url >>= fun reply ->
  expect_content_type reply "application/json" >>= fun () ->
  let input = P.input_of_async_channel reply.body in
  json_parse_tree input >>= function
  | [`O obj] ->
    begin match filter_field owner obj with
      | [`F (_, [ `A privileges ])] ->
        return (List.exists (function | `String "owner" -> true | _ -> false) privileges)
      | [] ->
        return false
      | p ->
        warning "bad acl list format: %a" pp_json_lst p;
        fail (Failure "bad acl list format")
    end
  | p ->
    warning "bad acl list format: %a" pp_json_lst p;
    fail (Failure "bad acl list format2")

let delete ?async url =
  Lwt.catch (fun () ->
      get_vol_nodelist url >>= fun (nodes, _) ->
      begin match url_path url ~encoded:true with
        | "" :: volume :: ([""] | []) ->
          let volume_url = Neturl.modify_url ~path:["";volume] url in
          if !Config.volume_create_elevate_to_admin then
            is_owner_of_vol nodes volume_url >>= function
            | true ->
              (* elevate to admin privileges for volume delete *)
              return (Neturl.modify_url volume_url ~user:!Config.key_id)
            | false ->
              return url
          else
            (* not owner or escalation not permitted: use current user *)
            return url
        | _ -> return url
      end >>= fun url ->
      make_request `DELETE nodes url >>=
      job_get ?async url
    ) (function
      | XIO.Detail (Unix.Unix_error((Unix.ENOENT|Unix.ENOTEMPTY),_,_) as e, _) ->
        fail e
      | e -> fail e)
