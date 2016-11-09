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

(* List Multipart Uploads
   DELETE Object
   DELETE Multiple Objects
   GET Object
   HEAD Object
   PUT Object
   PUT Object - COpy
   Initiate multipart upload
   Abort multipart upload
   List parts *)

module Delete = struct
  let policy = Policy.Permission.bucket "s3:DeleteObject"
end

module Head = struct
  let policy = Policy.Permission.obj "s3:GetObject"
end

module Get = struct
  let policy = Head.policy
end

module DeleteVersion = struct
  let policy = Policy.Permission.obj "s3:DeleteObjectVersion"
end

module HeadVersion = struct
  let policy = Policy.Permission.obj "s3:GetObjectVersion"
end

module GetVersion = struct
  let policy = HeadVersion.policy
end

module Put = struct
  let policy = Policy.Permission.bucket "s3:PutObject"
end

module PutCopy = struct
  let policy = Put.policy
end

module Post = struct
  let policy = Put.policy
end

module InitiateMultipart = struct
  type t
  let policy = Put.policy
end

module UploadPart = struct
  type t
  let policy = Put.policy
end

module CompleteMultipart = struct
  type t
  let policy = Put.policy
end

module DeleteMulti = struct
  type t
end

module Copy = struct
  type t
end

module UploadPartCopy = struct
  type t
end

module AbortMultipart = struct
  type t
end

module ListParts= struct
  type t
end

module Acl = struct
  type t
  module Get = struct
    let policy = Policy.Permission.obj "s3:GetObjectAcl"
  end
  module GetVersion = struct
    let policy = Policy.Permission.obj "s3:GetObjectVersionAcl"
  end
  module Put = struct
    let policy = Policy.Permission.obj "s3:PutObjectAcl"
  end
  module PutVersion = struct
    let policy = Policy.Permission.obj "s3:PutObjectVersionAcl"
  end
end
