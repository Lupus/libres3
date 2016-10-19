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

open Json_encoding
open Jsonenc

type target = Cluster | Volume | Block | SingleHost

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

let hex_to_json (`Hex h) = h
let hex_of_json j = `Hex j
let hex_encoding = conv hex_to_json hex_of_json string

module Meta = struct
  type t = (string * Hex.t) list

  let encoding = assoc hex_encoding

  let pp_hex ppf h =
    Fmt.pf ppf "%a" Fmt.lines (Hex.hexdump_s h)
      
  let pp = Fmt.(pair string pp_hex |> list)
end

module Job = struct
  module RequestId = struct
    type t = string
    let encoding = string
    let pp = Fmt.string
  end
  module Poll = struct
    type status = [`Pending | `Ok | `Error of string]
    type t = {
      request_id: RequestId.t;
      request_status: status;
    }
    let v (request_id, request_status, request_message) =
      match request_status with
      | (`Ok | `Pending) as v -> {request_id;request_status=v}
      | `Error -> {request_id;request_status=`Error request_message}

    let of_v t = match t.request_status with
    | (`Ok | `Pending) as v -> t.request_id,v,""
    | `Error msg -> t.request_id,`Error,msg

    let encoding = obj3
        (req "requestId" RequestId.encoding)
        (req "requestStatus" (string_enum [
             "PENDING", `Pending;
             "OK", `Ok;
             "ERROR", `Error
           ]))
        (req "requestMessage" string) |> obj_opt |> conv of_v v

    let example = "{\"requestId\":\"opaqueXYZ\",\"requestStatus\":\"ERROR\",\"requestMessage\":\"Job X failed\"}"                    
    let get id = Uri.make ~path:("/.results/" ^ id) ()

    let pp_status ppf = function
    | `Ok -> Fmt.pf ppf "OK"
    | `Pending -> Fmt.pf ppf "PENDING"
    | `Error msg -> Fmt.pf ppf "@[ERROR %s@]" msg

    let pp ppf t =
      Fmt.pf ppf "@[requestId: %a@, status: %a@]"
        RequestId.pp t.request_id pp_status t.request_status

    let target = SingleHost
  end

  type t = {
    request_id: RequestId.t;
    min_poll_interval: float;
    max_poll_interval: float;
  }

  let of_v t = t.request_id,t.min_poll_interval,t.max_poll_interval
  let v (request_id,min_poll_interval,max_poll_interval) = {
    request_id;min_poll_interval;max_poll_interval
  }

  let encoding = obj3 (req "requestId" RequestId.encoding)
      (req "minPollInterval" float)
      (req "maxPollInterval" float) |> obj_opt |> conv of_v v

  let pp _ = failwith "TODO"

  let target = SingleHost

  let example = "{\"requestId\":\"4\",\"minPollInterval\":100,\"maxPollInterval\":6000}"
end

module User = struct
  type t = User of string
  let v u =
    if String.contains u '/' then
      invalid_arg "Username cannot contain /";
    User u
  let of_v (User u) = u
  let encoding = conv of_v v string
  let pp = Fmt.(using of_v string)

  let uri u =
    let path = "/.users/" ^ (of_v u) in
    Uri.make ~path ()
end

module UploadToken = struct
  type t = string
  let encoding = string
  let pp = Fmt.string

  let block_uri ~blocksize token =
    let path = String.concat "/" ["/.data"; string_of_int blocksize; token] in
    Uri.make ~path ()

  let file_uri token =
    Uri.make ~path:("/.upload/" ^ token) ()
end

type query = (string * string list) list
let query_opt opt f lst = match opt with
| None -> lst
| Some v -> List.rev_append (f v) lst

let query_opt_bool v param q =
  if v then
    (param,[]) :: q
  else q

module Pattern = struct
  open Astring
  type t = string

  let is_wildcard c = c = '*' || c = '?' || c = '['

  let of_literal pattern =
    if String.exists is_wildcard pattern then
      let b = Buffer.create (String.length pattern) in
      String.iter (function
        | ('*' | '?' | '[') as c ->
            Buffer.add_char b '\\';
            Buffer.add_char b c;
        | c -> Buffer.add_char b c
        ) pattern;
      Buffer.contents b
    else pattern

  let of_glob pattern = pattern
  let add_query_opt p q = match p with
  | None -> q
  | Some pattern ->
      ("filter",[pattern]) :: q

  let pp = Fmt.string
end
