(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2016 Skylable Ltd. <info-copyright@skylable.com>   *)
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

open Rresult

module Id : sig
  type t = string
end

module CanonicalUser : sig
  type t = {
    id : Id.t;
    display_name: string;
  }
  val to_xml : t -> Xmlio.xml list
end

module Groups : sig
  type t = [`AuthenticatedUsers | `AllUsers | `LogDelivery]
  val of_uri : Uri.t -> (t, R.msg) result
end

module Grantee : sig
  type t =
    | Owner | ObjectOwner
    | EmailAddress of string
    | ID of Id.t
    | Uri of Groups.t
end

module T : sig
  type t = Read | Write | Read_acp | Write_acp
  val compare : t -> t -> int
end

module AclSet : Set.S with type elt = T.t
module GrantMap : Map.S with type key = Grantee.t
type t = AclSet.t GrantMap.t

val is_full_control : AclSet.t -> bool

val of_canned : string -> t
val of_header : Cohttp.Header.t -> (t, R.msg) result

module GetBucket : sig
  val policy : Policy.bucket Policy.Permission.t
end

module PutBucket : sig
  val policy : Policy.bucket Policy.Permission.t
end
