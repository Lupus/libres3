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

open SXDefaultIO
type url

type output_data = string * int * int
type input_stream = unit -> output_data Lwt.t
type output_stream = output_data -> unit Lwt.t

type metafn = unit -> (string * string) list

type sink = int64 -> output_stream Lwt.t

(* auth (note: the urls below should all have a user part too) *)
val token_of_user : [< `Url of url ] -> string option Lwt.t
val invalidate_token_of_user : [< `Url of url ] -> unit

(* operations *)
val get_meta: [< `Url of url] -> (string*string) list Lwt.t
val copy: ?quotaok:(unit->unit) -> ?metafn:metafn ->
  [< `Source of source | `Url of url | `Urls of url list * int64] -> srcpos:int64 ->
  [< `Sink of sink | `Url of url] -> unit Lwt.t
val delete: ?async:bool -> [< `Url of url ] -> unit Lwt.t

(* create a volume, directory, or file as appropiate *)
val create: ?replica:int -> [< `Url of url ] -> unit Lwt.t
val fold_list: base:[< `Url of url] -> [< `Url of url ] -> ?marker:string -> ?limit:int -> ?no_recurse:bool ->
  entry:('a -> entry -> 'a Lwt.t) -> recurse:(string -> bool) -> 'a -> 'a Lwt.t

val exists: [< `Url of url ] -> bool Lwt.t
val check : [< `Url of url ] -> string option Lwt.t

val set_acl: [< `Url of url] -> acl list -> unit Lwt.t
val get_acl: [< `Url of url] -> acl list Lwt.t
val create_user: [< `Url of url ] -> string -> string Lwt.t

(* sources *)
val of_source: source -> [> `Source of source]
val of_string: string -> [> `Source of source ]

(* sinks *)
val of_sink: sink -> [> `Sink of sink ]

(* urls *)
val of_url : string -> [> `Url of url]
val of_neturl: Neturl.url -> [> `Url of url]
val with_url_source: [< `Url of url] -> (source -> 'a Lwt.t) -> 'a Lwt.t
val with_urls_source: [< `Url of url] list -> int64 -> (source -> 'a Lwt.t) -> 'a Lwt.t

module type SchemeOps = sig
  type state
  type read_state
  val scheme : string
  val syntax: Neturl.url_syntax

  val token_of_user : Neturl.url -> string option Lwt.t
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
  val put: ?quotaok:(unit->unit) -> ?metafn:metafn -> source -> int64 -> Neturl.url -> unit Lwt.t
  val delete: ?async:bool -> Neturl.url -> unit Lwt.t
  val create: ?metafn:metafn -> ?replica:int -> Neturl.url -> unit Lwt.t
  val exists: Neturl.url -> bool Lwt.t

  val set_acl : Neturl.url -> acl list -> unit Lwt.t
  val get_acl : Neturl.url -> acl list Lwt.t
  val create_user: Neturl.url -> string -> string Lwt.t

  val fold_list: Neturl.url -> ?marker:string -> ?limit:int -> ?no_recurse:bool -> ('a -> entry -> 'a Lwt.t) -> (string -> bool) -> 'a -> 'a Lwt.t
end
module RegisterURLScheme(O: SchemeOps) : sig
  val register: unit -> unit
end
