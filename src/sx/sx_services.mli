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

open Services
open Sx_types
open Jsonenc
open Cohttp


module SX : sig
  type 'a req = 'a * Request.t * Body.t
  type resp = Http.resp
  type 'a t = ('a req, resp) Generic.t
  type ('a, 'b) filter = 'a t -> 'b t
end

type ('header, 'element) streaming = 'header * 'element list

type stream

type 'reply req =
  | WaitJob : job -> unit req
  | ListNodes : unit -> Sx_cluster.ListNodes.t req
  | GetClusterMeta : unit -> Sx_cluster.Meta.Get.t req
  | SetClusterMeta : Sx_cluster.Meta.Set.t -> job req
  | ListUsers : unit -> Sx_cluster.Users.List.t req
  | CreateUser : Sx_cluster.Users.Create.t -> job req
  | Self : unit -> Sx_cluster.Users.Self.t req
  | ModifyUser : Sx_cluster.Users.Modify.t -> job req
  | RemoveUser : Sx_types.User.t -> job req
  | RemoveUserClones : Sx_types.User.t -> job req
  | ListVolumes : unit -> Sx_volume.ListVolumes.t req
  | Locate : Sx_volume.T.t -> Sx_volume.Locate.t req
  | LocateUpload : (Int53.t * Sx_volume.T.t) -> Sx_volume.Locate.t req
  | CreateVolume : (Sx_volume.T.t * Sx_volume.Create.t) -> job req
  | ModifyVolume : Sx_volume.T.t * Sx_volume.Modify.t -> job req
  | ModifyReplica : Sx_volume.T.t * Sx_volume.ModifyReplica.t -> job req
  | DeleteVolume : Sx_volume.T.t -> job req
  | GetVolumeAcl : Sx_volume.T.t -> Sx_volume.Acl.Get.t req
  | UpdateVolumeAcl : Sx_volume.T.t -> Sx_volume.Acl.Update.t req
  | ListFiles : Sx_volume.T.t -> (Sx_volume.ListFiles.header,Sx_volume.ListFiles.element) streaming req
  | MassDelete : Sx_volume.T.t * Pattern.t -> job req
  | MassRename : Sx_volume.T.t * Sx_volume.Mass.Rename.t -> job req
  | GetFile : Sx_volume.T.t * string -> stream req
  | InititializeFile : Sx_volume.T.t *
                       (Sx_file.Initialize.Request.header, Sx_file.Initialize.Request.element) streaming ->
    (Sx_file.Initialize.Reply.header, Sx_file.Initialize.Reply.element) streaming req
  | AddChunk : Sx_file.Initialize.AddChunk.t -> (Sx_file.Initialize.Reply.header, Sx_file.Initialize.Reply.element) streaming req
  | FlushFile : UploadToken.t -> job req
  | DeleteFile : Sx_volume.T.t * string -> job req
  | ListRevisions: Sx_volume.T.t * string -> Sx_file.ListRevisions.t req
  | GetBlock : (int * Sx_block.t * Sx_block.t list) -> stream req
  | CreateBlock : (int * UploadToken.t) -> unit req
