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

open S3_types
open Rresult

module Region: sig
  type t = private string
  val pp : t Fmt.t
  val is_classic : t option -> bool
end

module Service : sig
  module Bucket : sig
    type t = {
      name: string;
      creation_date: CalendarLib.Calendar.t;
    }
  end
  type t = {
    owner : Acl.CanonicalUser.t;
    buckets: Bucket.t list;
  }

  include S with type t := t and type kind = Policy.bucket
end

module Create : sig
  type t = {
    bucket : string;
    acl : Acl.t;
    location_constraint: Region.t option;
  }

  val of_request : Cohttp.Header.t * string * Xmlio.xml option -> (t, R.msg) result
  (*  include S with type t := t and type kind := Policy.bucket*)
end

module StorageClass : sig
  type t = private string
  val to_string : t -> string
end

module ListObjects : sig
  type version = [`V1 | `V2]
  module Req : sig
    type t = {
      delimiter : string;
      encoding_type: Uri.t option;
      marker: string option;
      max_keys: int;
      prefix: string;
      list_type : version;
      continuation_token : string option;(* V2 *)
      fetch_owner: bool;(* V2 *)
      start_after: string option; (* V2 *)
    }
  end
  module Reply : sig
    type contents = {
      owner: Acl.CanonicalUser.t option;(* V2: optional *)
      etag: string;
      key: string;
      last_modified: CalendarLib.Calendar.t;
      size: Int64.t;
      storage_class: StorageClass.t;
    }
    type t = {
      contents: contents list;
      common_prefixes : string list; (* counts as 1 towards max_keys *)
      delimiter: string option;
      encoding_type: string option;
      is_truncated: bool;
      marker: string option; (* V1 *)
      max_keys: int;
      name: string;
      next_marker: string option; (* V1 *)
      owner: Acl.CanonicalUser.t option; (* V2: optional *)
      prefix: string option;
      continuation_token: string option;(* V2 *)
      key_count: int option; (* V2 *)
      next_continuation_token: string option;(* V2 *)
      start_after: string option; (* V2 *)
    }
    include S with type t := t and type kind = Policy.bucket
  end
end

module ListMultipartUploads : sig
  type t
  val policy : Policy.bucket Policy.Permission.t
end

module GetObjectVersions : sig
  val policy : Policy.bucket Policy.Permission.t
end
