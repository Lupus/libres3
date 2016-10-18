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
module ListNodes : sig
  type t = { node_list:Ipaddr.t list }
  include JsonGetQuery with type t := t
end

module Meta: sig
  type t = { cluster_meta: Sx_types.Meta.t }
  module Get : JsonGetQuery with type t = t
  module Set : JobQuery with type t = t
end

module Users : sig
  module Key : sig
    type t = Key of Hex.t
    include Convertible with type t := t
  end
  type attr = {
    admin: bool;
    user_quota: Int53.t option;
    user_quota_used: Int53.t option;
    user_desc : string option;
  }
  type t = (User.t * attr) list
  module List : JsonQuery with type t = t
  module Create : sig
    type t = {
      user_name: User.t;
      user_key: Key.t;
      attr: attr;
      existing_name: User.t option;
    }
    include JobQuery with type t := t
  end
  module Self : JsonQuery with type t = User.t * attr
  module Modify : sig
    type t
    include JobQuery with type t := t
  end
  module Remove : sig
    val target : target
    val delete : ?all_clones:bool -> User.t -> Uri.t
  end
end
