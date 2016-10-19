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

open Sx_types
type t = private string

val of_hex : Hex.t -> t
val to_hex : t -> Hex.t
val encoding : t Json_encoding.encoding

val of_string : string -> t
val to_string : t -> string
val pp : t Fmt.t

val unsafe_of_assoc : (string * 'a) list -> (t * 'a) list
val unsafe_to_assoc : (t * 'a) list -> (string * 'a) list

open Jsonenc
val unsafe_of_streaming : ('a, (string * 'b)) streaming -> ('a, (t * 'b)) streaming
val unsafe_to_streaming : ('a, (t * 'b)) streaming -> ('a, (string * 'b)) streaming

module Get : sig
  val get : blocksize:int -> t -> t list -> Uri.t
  val target : target
end

module Create : sig
  val put : blocksize:int -> UploadToken.t -> Uri.t
  val target : target
end
