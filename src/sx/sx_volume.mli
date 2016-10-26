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
    privs: Sx_acl.RW.t;
    owner: User.t;
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
  val uri : t -> Uri.t
  val v : string -> t
   
end

module ListVolumes : sig
  type t = (T.t * Attr.t) list
  include JsonQuery with type t := t
  val get : ?volume_meta:bool -> ?custom_volume_meta:bool -> unit -> Uri.t
end

module Locate : sig
  type volnodes = Ipaddr.t list
  type size_info = {
    growable_size : Int53.t;
    block_size: int;
  }
  type t = volnodes * (size_info option * Attr.t)
  include JsonQuery with type t := t
  val get : ?volume_meta:bool -> ?custom_volume_meta:bool -> ?size:Int53.t -> T.t
    -> Uri.t
end

module Create : sig
  type t = {
    volume_size : Int53.t;
    owner: User.t;
    replica_count: int;
    max_revisions: int;
    volume_meta: Meta.t option;
  }
  include JobQuery with type t := t
  val put : T.t -> Uri.t
end

module Modify : sig
  type t = {
    size: Int53.t option;
    owner: User.t option;
    max_revisions: int option;
    custom_volume_meta: Meta.t option;
    name : T.t option
  }

  include JobQuery with type t := t
  
  val put : T.t -> Uri.t
end

module ModifyReplica : sig
  type t = {
    next_replica: int;
  }

  include JobQuery with type t := t
  
  val put : T.t -> Uri.t
end

module Delete : sig
  val delete : T.t  -> Uri.t
  val target : target
end

module Acl : sig
  module Get : sig
    type t = (User.t * Sx_acl.t) list
    include JsonQuery with type t := t
    val get : T.t -> Uri.t
  end
  module Update : sig
    type t = {
      grant_read : User.t list;
      grant_write: User.t list;
      revoke_read : User.t list;
      revoke_write: User.t list;
    }
    include JobQuery with type t := t
    val put : T.t -> Uri.t
  end
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

  type element = string * t

  val pp : t Fmt.t

  val all_encoding : (Int53.t * (string * t) list) Jsonenc.encoding

  val streaming : (header, element) Jsonenc.streaming

  val get : ?filter:Pattern.t -> ?recursive:bool -> ?limit:int -> ?after:string -> T.t
    -> Uri.t

  val example : string
end

module Mass : sig
  module Delete : sig
    val delete : T.t -> Pattern.t -> Uri.t
    val target : target
  end
  module Rename : sig
    type t = {
      source: string;
      dest: string;
      recursive: bool;
    }
    val rename : T.t -> t -> Uri.t
  end
end
