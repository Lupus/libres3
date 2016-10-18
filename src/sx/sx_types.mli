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

type target = Cluster | Volume

module type Convertible = sig
  type t
  val encoding : t encoding
  val pp : t Fmt.t
end

module type JsonQuery = sig
  include Convertible
  val target : target
  val example : string
end

module type JsonGetQuery = sig
  include JsonQuery
  val get : Uri.t
end

module type JobQuery = JsonQuery

module type JobPutQuery = sig
  include JobQuery
  val put : Uri.t
end

module type JobDeleteQuery = sig
  val target : target
  val delete : Uri.t
end

val hex_encoding : Hex.t encoding

module Meta : sig
  type t = (string * Hex.t) list
  val encoding : t encoding
  val pp : t Fmt.t
end

type query = (string * string list) list
val query_opt : 'a option -> ('a -> query) -> query -> query
val query_opt_bool : bool -> string -> query -> query
