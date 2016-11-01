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
open Boundedio
open S3_service

module SX = Sx_service

let get_custom_meta attrs field conv default =
  let open Sx_volume in
  match attrs.Attr.custom_volume_meta with
  | None -> default
  | Some (Meta.VolumeMeta meta) ->
      try List.assoc field meta |> Hex.to_string |> conv
      with Not_found -> default

let attr_creation = "libres3-creation-date"

let wait_job job =
  SX.sx_retry SX.service (Sx_services.WaitJob job)

(* TODO: no GADT, use first class modules or functors *)

let libres3_hidden_prefix = ".libres3-dontmodify-"
let libres3_hidden_key = "libres3-xml"

let service : type a. a req -> a Boundedio.t = function
| GetService _ ->
    SX.service (ListVolumes ()) >>= fun volumes ->
    (* TODO: strict : list only my own buckets or all that have access to *)
    let open Bucket in
    let bucket_of_volume (Sx_volume.T.Volume name, attr) =
      let creation_date =
        get_custom_meta attr attr_creation float_of_string 0. |>
        CalendarLib.Calendar.from_unixfloat in
      {
        Service.Bucket.name = name;
        creation_date
      }
    in
    let owner = {
      Acl.CanonicalUser.id = "00";
      display_name = "test";
    }
    in
    return Service.{
      owner;
      buckets = List.rev_map bucket_of_volume volumes
      }

| CreateBucket (_, bucket, data) ->
    let owner = Sx_types.User.v "admin" in (*  TODO *)
    let bucket = R.get_ok (
      begin if Bucket.Region.is_classic data.location_constraint then
        S3_validate.is_classic_compliant bucket
        else S3_validate.is_dns_compliant bucket
      end) in
      let d = Unix.gettimeofday () |> string_of_float |> Hex.of_string in
      let attr = {
        Sx_volume.Create.volume_size = (Jsonenc.Int53.of_int64_exn 1048576L)
          (* get_cluster_meta "libres3-default-volume-size" cm *);
        owner;
        replica_count = 1 (* get_cluster_meta "libres3-replica-count" cm *);
        max_revisions = 1; (* not versioning enabled by default *)
        volume_meta = Some (Sx_volume.Meta.VolumeMeta ["libres3-creation-date", d])
      } in
    let vol = Sx_volume.T.v bucket in
    SX.service (CreateVolume (Sx_volume.T.v bucket, attr)) >>= wait_job >>> (function
    | Ok () -> return_unit
    | Error (SX.JobError _ as e) ->
        SX.service (Locate vol) >>= fun (_,(_, attr)) ->
        if attr.Sx_volume.Attr.owner = owner then return_unit (* EEXIST = OK *)
        else fail e
    | Error e -> fail e)
| DeleteBucket (_, bucket) ->
    SX.sx_retry SX.service (DeleteVolume (Sx_volume.T.v bucket)) >>= wait_job
| PutBucketSubresource (_, bucket, subresource, xml) ->
    let open Sx_file in
    let vol = Sx_volume.T.v bucket in
    let str = Xmlio.to_string xml in
    let hidden = libres3_hidden_prefix ^ subresource in
    let meta = { Meta.file_meta = [libres3_hidden_key, Hex.of_string str] } in
    SX.service (InitializeFile (vol, hidden, ((Jsonenc.Int53.of_int64_exn 0L, meta), []))) >>= fun (token, _) ->
    SX.service (FlushFile token) >>= wait_job

| DeleteBucketSubresource (_, bucket, subresource) ->
    let open Sx_file in
    let vol = Sx_volume.T.v bucket in
    let hidden = libres3_hidden_prefix ^ subresource in
    SX.service (DeleteFile (vol, hidden)) >>= wait_job

| GetBucketSubresource (_, bucket, subresource) ->
    let open Sx_file in
    let vol = Sx_volume.T.v bucket in
    let hidden = libres3_hidden_prefix ^ subresource in
    SX.service (GetFileMeta (vol, hidden)) >>= fun meta ->
    begin match List.hd meta.file_meta |> snd |> Hex.to_string |> Xmlio.of_string with
    | Some s -> return (s:Xmlio.xml)
    | None -> fail (Failure "no xml")
      end

| _ -> fail (Failure "not implemented")
