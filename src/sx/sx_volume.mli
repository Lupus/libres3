(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2016 Skylable Ltd. <info-copyright@skylable.com>   *)
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

open Jsonenc
open Sx_types

module Meta : sig
  type t = VolumeMeta of Sx_types.Meta.t
  include Convertible with type t := t
end

module Attr : sig
  type t = {
    size_bytes: Int53.t;
    used_size : Int53.t;
    replica_count: int;
    max_revisions: int;
    privs: Sx_acl.Privs.t;
    owner: Sx_cluster.User.t;
    files_size: Int53.t;
    files_count: Int53.t;
    volume_meta: Meta.t option;
    custom_volume_meta: Meta.t option;
    global_id: string option;
  }
  include Convertible with type t := t
end

module T : sig
  type t = Volume of string
  include Convertible with type t := t
end

module List : sig
  type t = (T.t * Attr.t) list
  include JsonQuery with type t := t
end

module Locate : sig
 type t = {
    node_list : Ipaddr.t list;
    block_size : int option;
    volume_meta : Meta.t option;
    custom_volume_meta : Meta.t option;
    files_size : Int53.t;
    files_count: Int53.t;
    size_bytes : Int53.t;
    used_size : Int53.t;
    replica_count : int;
    max_revisions : int;
    privs: string;
    owner: string;
    global_id: string option;
  }
  include JsonGetQuery with type t := t
end

module ListFiles : sig
  type header = {
    volume_size: Int53.t;
  }

  type attributes = {
    file_size : Int53.t;
    block_size: int;
    created_at : Http_date.t;
    file_revision: string;
  }

  type t = File of attributes | Directory

  val pp : t Fmt.t

  val all_encoding : (Int53.t * (string * t) list) Jsonenc.encoding

  val streaming : (header, t) Jsonenc.streaming

  val get : ?filter:string -> ?recursive:bool -> ?limit:int -> ?after:string ->
    volume:string -> Uri.t

  val example : string
end
