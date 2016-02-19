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

open Lwt
open SXDefaultIO
type metafn = unit -> (string * string) list
type output_data = string * int * int
type input_stream = unit -> output_data t
type output_stream = output_data -> unit t

type sink = int64 -> output_stream t

let read_string str ?len pos =
  let eof = ref false in
  return (fun () ->
      if !eof then return ("",0,0)
      else begin
        eof := true;
        let len = match len with
          | None -> Int64.sub (Int64.of_int (String.length str)) pos
          | Some len -> len in
        if len < 0L then return ("",0,0)
        else return (str, Int64.to_int pos, Int64.to_int len)
      end
    );;

(* sources *)
let of_string str = `Source {
    meta = {
      name = "";
      size = Int64.of_int (String.length str);
      mtime = Unix.gettimeofday ();
      etag = Digest.to_hex (Digest.string str)
    };
    seek = read_string str;
  }
let of_source src = `Source src
(*  let of_fd_in ch =
    `Source ch;;*)

(* sinks *)
let of_sink dst = `Sink dst
(*  let of_fd_out ch =
    `Sink ch;;*)

(* URLs *)
type url = Neturl.url

let scheme_re = Netstring_str.regexp "^\\([a-zA-Z][a-zA-Z0-9+.-]*\\)://"
let schemes = Hashtbl.create 16

type op = {
  with_url_source : 'a. url -> (source -> 'a t) -> 'a t;
  with_urls_source : 'a. url list -> int64 -> (source -> 'a t) -> 'a t;
  fold_list: 'a. url -> ?etag:string -> ?marker:string -> ?limit:int -> ?no_recurse:bool -> ('a -> entry -> 'a t) -> (string -> bool) -> 'a -> 'a cond t;
  create: ?metafn:metafn -> ?replica:int -> url -> unit t;
  exists: url -> bool t;
  token_of_user: url -> string option t;
  invalidate_token_of_user: url -> unit;
  check: url -> string option t;
  delete: ?async:bool -> url -> unit t;
  copy_same: ?metafn:metafn -> ?filesize:int64 -> url list -> url -> bool t;
  get_meta: url -> (string*string) list t;
  set_meta: url -> (string*string) list -> unit t;
  with_settings : url -> max_wait:float -> (string -> string option Lwt.t) -> string -> unit Lwt.t;
  put: ?quotaok:(unit->unit) -> ?metafn:metafn -> source -> int64 -> url -> unit t;
  set_acl: url -> acl list -> unit t;
  get_acl: url -> acl list t;
  create_user: url -> string -> string t;
}
let scheme_ops : (string,op) Hashtbl.t = Hashtbl.create 16

let encode_8bits s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
      let n = Char.code c in
      if n >= 0x80 then
        Buffer.add_string b (Printf.sprintf "%%%02X" n)
      else
        Buffer.add_char b c
    ) s;
  Buffer.contents b;;

open Neturl
let path_norm_encode f components =
  norm_path (List.rev (List.rev_map f components));;

let encode_unsafe str = Neturl.fixup_url_string (encode_8bits str);;

let encode_opt f url =
  try Some (encode_8bits (f ?encoded:(Some true) url))
  with Not_found -> None;;

let encode_opt_list f url =
  try
    Some (List.map encode_8bits (f ?encoded:(Some true) url))
  with Not_found ->
    None;;

let normalize_url url =
  let syntax = url_syntax_of_url url in
  let path_encoder =
    if syntax.url_enable_query = Url_part_not_recognized then
      Netencoding.Url.encode ~plus:true (* fully encode, input is not encoded *)
    else
      encode_unsafe (* encode just unsafe and 8bits *) in
  let url = Neturl.remove_from_url ~fragment:true ~other:true url in
  Neturl.modify_url ~encoded:true
    ?user:(encode_opt url_user url)
    ?user_param:(encode_opt_list url_user_param url)
    ?password:(encode_opt url_password url)
    ~path:(path_norm_encode path_encoder (url_path ~encoded:true url))
    ?param:(encode_opt_list url_param url)
    ?query:(encode_opt url_query url)
    url;;

let of_url str =
  `Url (match Netstring_str.string_match scheme_re str 0 with
      | Some result ->
        let scheme = Netstring_str.matched_group result 1 str in
        if Hashtbl.mem schemes scheme then
          let url =
            Neturl.parse_url ~schemes ~accept_8bits:true ~enable_fragment:true str in
          normalize_url (Neturl.ensure_absolute_url url)
        else
          failwith ("Unsupported URL scheme: " ^ scheme)
      | None ->
        (* URL without scheme: convert to file:// URL *)
        Neturl.file_url_of_local_path str);;

(*let details = Hashtbl.find scheme_ops (url_scheme url) in*)
let of_neturl url =
  `Url (Neturl.ensure_absolute_url url);;

let ops_of_url url =
  let scheme = url_scheme url in
  try
    Hashtbl.find scheme_ops scheme
  with Not_found ->
    (* TODO: use fail *)
    raise (Failure ("Unregistered URL scheme: " ^ scheme));;

let with_url_source (`Url url) f =
  (ops_of_url url).with_url_source url f;;

let with_urls_source urls filesize f =
  match urls with
  | [] ->
    let `Source s = of_string "" in
    f s
  | (`Url url) :: _ ->
    (* TODO: check they all have same scheme *)
    let urls = List.rev (List.rev_map (fun (`Url url) -> url) urls) in
    (ops_of_url url).with_urls_source urls filesize f;;

let remove_base base str =
  let n = String.length str in
  let baselen = String.length base in
  if String.sub str 0 baselen = base then
    String.sub str baselen (n - baselen)
  else
    "";;

let fold_list ~base:(`Url base) (`Url url) ?etag ?marker ?limit ?no_recurse ~entry ~recurse =
  let prefix = join_path (url_path base) in
  let fold_entry accum e =
    entry accum {
      e with
      name = remove_base prefix e.name
    } in
  let fold_recurse dir =
    recurse (remove_base prefix dir) in
  (ops_of_url url).fold_list url ?etag ?marker ?limit ?no_recurse fold_entry fold_recurse;;

let create ?replica (`Url url) =
  (ops_of_url url).create ?replica url;;

let exists (`Url url) =
  (ops_of_url url).exists url;;

let token_of_user (`Url url) =
  (ops_of_url url).token_of_user url;;

let invalidate_token_of_user (`Url url) =
  (ops_of_url url).invalidate_token_of_user url;;

let check (`Url url) =
  (ops_of_url url).check url;;

let set_acl (`Url url) acls = (ops_of_url url).set_acl url acls
let get_acl (`Url url) = (ops_of_url url).get_acl url
let create_user (`Url url) name = (ops_of_url url).create_user url name

let delete ?async (`Url url) =
  (ops_of_url url).delete ?async url;;

let put ?quotaok ?metafn dsturl source ~srcpos =
  (ops_of_url dsturl).put ?quotaok ?metafn source srcpos dsturl;;

let with_src src f = match src with
  | `Source source -> f source
  | `Url _ as srcurl -> with_url_source srcurl f
  | `Urls (url :: _ as urls, filesize) ->
    (ops_of_url url).with_urls_source urls filesize f
  | `Urls ([], _ ) ->
    let `Source s = of_string "" in f s

let callopt = function
  | Some fn -> fn ()
  | None -> ()

let generic_copy ?quotaok ?metafn src ~srcpos dst = match dst with
  | `Url dsturl ->
    with_src src (put ?quotaok ?metafn dsturl ~srcpos)
  | `Sink (sink:sink) ->
    sink 0L >>= fun out ->
    with_src src (fun source ->
        source.seek srcpos >>= fun stream ->
        callopt quotaok;
        iter stream out
      );;

let get_meta (`Url src) =
  (ops_of_url src).get_meta src

let set_meta (`Url src) meta =
  (ops_of_url src).set_meta src meta

let with_settings (`Url src) ~max_wait f key =
  (ops_of_url src).with_settings src ~max_wait f key

let url_port_opt u = try Some (url_port u) with Not_found -> None
let same_cluster a b =
  url_host a = url_host b &&
  url_port_opt a = url_port_opt b &&
  url_scheme a = url_scheme b
let same_clusters lst b = List.for_all (same_cluster b) lst

let copy ?quotaok ?metafn src ~srcpos dst =
  begin match srcpos, src, dst with
    | 0L, `Urls (srcurls, filesize), (`Url dsturl) when same_clusters srcurls dsturl ->
      (ops_of_url dsturl).copy_same ?metafn ~filesize srcurls dsturl
    | 0L, (`Url srcurl), (`Url dsturl) when same_cluster srcurl dsturl ->
      (ops_of_url dsturl).copy_same ?metafn [ srcurl ] dsturl
    | _, (`Source _ | `Url _ | `Urls _), (`Sink _ | `Url _) -> return_false
  end >>= function
  | true -> callopt quotaok; return_unit
  | false ->
    generic_copy ?quotaok ?metafn src ~srcpos dst



module type SchemeOps = sig
  type state
  type read_state
  val scheme : string
  val syntax: Neturl.url_syntax

  val token_of_user: Neturl.url -> string option Lwt.t
  val invalidate_token_of_user : Neturl.url -> unit
  val check: Neturl.url -> string option Lwt.t
  val open_source: Neturl.url -> (entry * state) Lwt.t
  val seek: state -> ?len:int64 -> int64 -> (state * read_state) Lwt.t
  val read: (state * read_state) -> output_data Lwt.t
  val close_source : state -> unit Lwt.t

  (* true: optimized copy if scheme and authority matches
   * false: fallback to generic copy *)
  val copy_same: ?metafn:metafn -> ?filesize:int64 -> Neturl.url list -> Neturl.url -> bool Lwt.t

  val get_meta: Neturl.url -> (string*string) list Lwt.t
  val set_meta: Neturl.url -> (string*string) list -> unit Lwt.t
  val with_settings : Neturl.url -> max_wait:float -> (string -> string option Lwt.t) -> string -> unit Lwt.t
  val put: ?quotaok:(unit->unit) -> ?metafn:metafn -> source -> int64 -> Neturl.url -> unit Lwt.t
  val delete: ?async:bool -> Neturl.url -> unit Lwt.t
  val create: ?metafn:metafn -> ?replica:int -> Neturl.url -> unit Lwt.t
  val exists: Neturl.url -> bool Lwt.t
  val set_acl : Neturl.url -> acl list -> unit Lwt.t
  val get_acl : Neturl.url -> acl list Lwt.t
  val create_user: Neturl.url -> string -> string Lwt.t

  val fold_list: Neturl.url -> ?etag:string -> ?marker:string -> ?limit:int -> ?no_recurse:bool ->
    ('a -> entry -> 'a t) -> (string -> bool) -> 'a -> 'a cond Lwt.t
end

module RegisterURLScheme(O: SchemeOps) = struct
  let readurl state () = O.read state
  let seekurl state ?len pos =
    O.seek state ?len pos >>= fun read_state ->
    return (readurl read_state);;

  let withurl url f =
    O.open_source url >>= fun (entry, state) ->
    EventIO.try_finally
      (fun () ->
         f { meta = entry; seek = seekurl state }
      )
      (fun () ->
         O.close_source state
      ) ();;

  let rec read_urls current_source current_urls () =
    match !current_source with
    | Some ((state, _) as read_state) ->
      O.read read_state >>= fun (str, pos, len) ->
      if len = 0 then begin
        O.close_source state >>= fun () ->
        current_source := None;
        read_urls current_source current_urls ()
      end else
        return (str, pos, len)
    | None ->
      match !current_urls with
      | [] -> return ("", 0, 0)
      | url :: tl ->
        current_urls := tl;
        O.open_source url >>= fun (_, state) ->
        O.seek state 0L >>= fun read_state ->
        current_source := Some read_state;
        read_urls current_source current_urls ()

  let withurls urls filesize f =
    let current_source = ref None in
    let current_urls = ref urls in
    f {
      meta = { name = ""; size = filesize; mtime = 0.; etag = "" };
      seek = (fun ?len pos ->
          if pos <> 0L then
            fail (Failure "Non-seekable stream")
          else
            return (read_urls current_source current_urls)
        )
    }

  let ops = {
    with_url_source = withurl;
    with_urls_source = withurls;
    fold_list = O.fold_list;
    token_of_user = O.token_of_user;
    invalidate_token_of_user = O.invalidate_token_of_user;
    create = O.create;
    exists = O.exists;
    check = O.check;
    delete = O.delete;
    copy_same = O.copy_same;
    put = O.put;
    get_meta = O.get_meta;
    set_meta = O.set_meta;
    with_settings = O.with_settings;
    create_user = O.create_user;
    set_acl = O.set_acl;
    get_acl = O.get_acl;
  }

  let register () =
    Hashtbl.add scheme_ops O.scheme ops;
    Hashtbl.add schemes O.scheme O.syntax;;
end

let sxio_printer = function
  | Unix.Unix_error(code,fn,arg) ->
    Some (Printf.sprintf "%s(%s): %s" fn arg
            (Unix.error_message code))
  | Detail (e, lst) ->
    let buf = Buffer.create 128 in
    Buffer.add_string buf (Printexc.to_string e);
    Buffer.add_char buf '\n';
    List.iter (fun (k,v) ->
        Buffer.add_char buf '\t';
        Buffer.add_string buf k;
        Buffer.add_string buf " -> ";
        Buffer.add_string buf v;
        Buffer.add_char buf '\n') lst;
    Some (Buffer.contents buf)
  | _ ->
    None;;

let () =
  Printexc.register_printer sxio_printer

module RegisterFile = RegisterURLScheme(SXFile)
module RegisterSX = RegisterURLScheme(SXC)
let () =
  RegisterFile.register ();
  RegisterSX.register ()
