(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2015 Skylable Ltd. <info-copyright@skylable.com>   *)
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

open CodedIO

type effect = Allow | Deny

module type Element = sig
  type t
  val name: string
  val of_string  : string -> t
  val matches : element:t -> t -> bool
end

module Elements(E: Element) = struct
  type t = Incl of E.t list | Excl of E.t list

  let map_uniq l = List.rev_map E.of_string (List.sort_uniq String.compare l)
  let of_lists = function
    | [], [] -> failwith ("Must specify either " ^ E.name ^ " or Not"^E.name)
    | incl, [] -> Incl (map_uniq incl)
    | [], excl -> Excl (map_uniq excl)
    | _ -> failwith ("Must specify either " ^ E.name ^ " or Not"^E.name)

  let matches element = function
    | Incl l -> List.exists (E.matches ~element) l
    | Excl l -> not (List.exists (E.matches ~element) l)
end

module Action = struct
  type t = string
  let name = "Action"
  let of_string s = s
  let matches ~element s = element = s
end

module Actions = Elements(Action)

module Principal = struct
  type t = Anon | AWS of string list | Federated of string | Service of string |
           CanonicalUser of string
  let of_string_json = function
    | `String "*" -> Anon
    | `O obj -> begin match obj with
        | ["AWS", a] -> AWS (Json.expect_array_or_string a)
        | ["Federated", v] -> Federated (Json.expect_string v)
        | ["Service", s] -> Service (Json.expect_string s)
        | ["CanonicalUser", u] -> CanonicalUser (Json.expect_string u)
        | [f, _] -> failwith ("Unknown principal type " ^ f)
        | _ -> failwith "Invalid principal JSON"
      end
    | _ -> failwith "Invalid principal JSON"
  let name = "Principal"
  let matches ~element = function
    | Anon -> true
    | p -> element = p
end

module Principals = struct
  type t = {
    incl: Principal.t list;
    excl: Principal.t list;
  }

  let of_lists (incl, excl) = { incl = incl; excl = excl }
  let matches element l =
    List.exists (Principal.matches ~element) l.incl &&
    not (List.exists (Principal.matches ~element) l.excl)
end

module Resource = struct
  type t = string list
  let colon = Netstring_str.regexp ":"
  let of_string s =
    match Netstring_str.bounded_split colon s 6 with
    | ["arn"; partition; service; _region; namespace; relativeid] ->
      if partition <> "aws" ||
         service <> "s3" then
        failwith ("Resource expected to be S3 in " ^ s);
      if namespace <> "" then
        failwith ("Namespace should be empty in resource" ^ s);
      Neturl.split_path relativeid
    | _ -> failwith ("Invalid resource " ^ s)

  let name = "Resource"

  let rec matches ~element p = match element, p with
    | s1 :: tl1, s2 :: tl2 when s1 = s2 ->
      matches ~element:tl1 tl2
    | s :: _, ["*"] when s <> "" -> true
    | _ -> false

  let is_bucket ~bucket = function
    | policy_bucket :: _ -> policy_bucket = bucket
    | _ -> false
end
module Resources = Elements(Resource)

type statement = {
  sid: string option;
  effect: effect;
  principals: Principals.t;
  actions: Actions.t;
  resources: Resources.t;
  condition: (string * Json.t) list;
}

type t = {
  version: string;
  id : string option;
  statements: statement list;
}

let empty = {
  version = "2008-10-17";
  id = None;
  statements = []
}

let valid p bucket =
  p.version = "2012-10-17" &&
  p.statements <> [] &&
  List.for_all (fun stmt ->
      match stmt.resources with
      | Resources.Incl r ->
        List.for_all (Resource.is_bucket ~bucket) r
      | _ -> false
    ) p.statements

module Stmt = struct
  type t = {
    sid: string option;
    effect: effect option;
    principal: Principal.t list;
    not_principal: Principal.t list;
    action: string list;
    not_action: string list;
    resource: string list;
    not_resource: string list;
    condition: (string * Json.t) list;
  }

  let empty = {
    sid = None;
    effect = None;
    principal = [];
    not_principal = [];
    action = [];
    not_action = [];
    resource = [];
    not_resource = [];
    condition = []
  }

  let parse_effect = function
    | `String "Allow" -> Allow
    | `String "Deny" -> Deny
    | _ -> failwith "Invalid Effect"

  let fold accum = function
    | "Sid", sid -> { accum with sid = Some (Json.expect_string sid) }
    | "Effect", e -> { accum with effect = Some (parse_effect e) }
    | "Principal", p ->
      { accum with principal = [Principal.of_string_json p] }
    | "NotPrincipal", p ->
      { accum with not_principal = [Principal.of_string_json p] }
    | "Action", a ->
      { accum with
        action = Json.expect_array_or_string a }
    | "NotAction", a ->
      { accum with
        not_action = Json.expect_array_or_string a }
    | "Resource", a ->
      { accum with resource = Json.expect_array_or_string a }
    | "NotResource", a ->
      { accum with not_resource = Json.expect_array_or_string a }
    | "Condition", c ->
      { accum with condition = (Json.expect_obj c) }
    | f, _ -> failwith ("Unknown field " ^ f)
end

let valid_statement s =
  match s.Stmt.effect with
  | None -> failwith "Invalid statement: effect not specified"
  | Some e ->
  {
    sid = s.Stmt.sid;
    effect = e;
    principals = Principals.of_lists (s.Stmt.principal, s.Stmt.not_principal);
    actions = Actions.of_lists (s.Stmt.action, s.Stmt.not_action);
    resources = Resources.of_lists (s.Stmt.resource, s.Stmt.not_resource);
    condition = s.Stmt.condition
  }

let map_statement json =
  valid_statement (List.fold_left Stmt.fold Stmt.empty (Json.expect_obj json))

let fold_toplevel accum = function
  | "Version", v -> { accum with version = Json.expect_string v }
  | "Id", id -> { accum with id = Some (Json.expect_string id) }
  | "Statement", stmt ->
    { accum with
      statements = List.rev_map map_statement (Json.expect_array stmt) }
  | f, _ -> failwith ("Unknown field " ^ f)

let of_string s =
  let json = Json.expect_obj (Json.of_string s) in
  List.fold_left fold_toplevel empty json

let is_anon_bucket_policy p bucket =
  valid p bucket && match p.statements with
  | [ { effect = Allow;
        principals = { Principals.incl = [Principal.Anon]; excl = [] };
        actions = Actions.Incl [ "s3:GetObject" ];
      } ]
    -> true
  | _ -> false
