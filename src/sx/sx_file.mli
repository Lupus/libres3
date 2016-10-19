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

module Revision : Convertible with type t = private string

module Get : sig
  type header = {
    file_size : Int53.t;
    block_size: int;
    created_at: Http_date.t;
    file_revision: Revision.t;
  }
  type t = Sx_block.t * Ipaddr.t list
  type element = t

  val all_encoding : ((Int53.t * int * Http_date.t * Revision.t) * t list) Jsonenc.encoding
  val streaming : (header, element) Jsonenc.streaming
  val get : Sx_volume.T.t -> string -> Uri.t
  val example : string
  val pp : t Fmt.t
end

module Meta : sig
  type t = { file_meta: Meta.t }
  include JsonQuery with type t := t
end

module Initialize : sig
  module Request : sig
    type header = Int53.t * Meta.t
    type t = Sx_block.t
    type element = t
    val all_encoding : (header * t list) Jsonenc.encoding
    val streaming : (header, element) Jsonenc.streaming
    val put : Sx_volume.T.t -> string -> Uri.t
    val example : string
    val pp : t Fmt.t
  end
  module Reply : sig
    type header = UploadToken.t
    type t = Ipaddr.t list
    type element = Sx_block.t * t
    val all_encoding : (header * element list) Jsonenc.encoding
    val streaming : (header, element) Jsonenc.streaming
    val example : string
    val pp : t Fmt.t
  end

  module AddChunk : sig
    type t = {
      extend_seq: Int53.t;
      file_data: Sx_block.t list;
      file_meta: (string * Hex.t option) list;
    }
    include JsonQuery with type t := t
    val put : UploadToken.t -> Uri.t
  end
end

module Flush : sig
  val put : UploadToken.t -> Uri.t
  val target : target
end

module Delete : sig
  val delete : Sx_volume.T.t -> string -> Uri.t
  val target : target
end

module ListRevisions: sig
  type attr = {
    block_size: int;
    file_size: Int53.t;
    created_at: Http_date.t;
  }
  type t = (Revision.t * attr) list
  include JsonQuery with type t := t
  val get : Sx_volume.T.t -> string -> Uri.t
end
