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
  val to_string: t -> string
end

let map_string s = `String s
let arr_or_string v : Json.t = match v with
  | [] -> `A []
  | [ one ] -> `String one
  | l -> `A (List.rev_map map_string l)

let arr_or_item = function
  | [] -> `A []
  | [ one ] -> one
  | l -> `A l

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

  let to_json key = function
    | Incl l -> key, arr_or_string (List.rev_map E.to_string l)
    | Excl l -> key, arr_or_string (List.rev_map E.to_string l)
end

module Action = struct
  type t = string
  let name = "Action"
  let of_string s = s
  let to_string s = s
  let matches ~element s = element = s
end

module Actions = Elements(Action)


module Principal = struct
  type t = Anon | AWS of string list | Federated of string | Service of string |
           CanonicalUser of string
  let of_string_json = function
    | `String "*" -> Anon
    | `O obj -> begin match obj with
        | ["AWS", a] -> AWS (CodedIO.Json.expect_array_or_string a)
        | ["Federated", v] -> Federated (CodedIO.Json.expect_string v)
        | ["Service", s] -> Service (CodedIO.Json.expect_string s)
        | ["CanonicalUser", u] -> CanonicalUser (CodedIO.Json.expect_string u)
        | [f, _] -> failwith ("Unknown principal type " ^ f)
        | _ -> failwith "Invalid principal JSON"
      end
    | _ -> failwith "Invalid principal JSON"
  let name = "Principal"
  let matches ~element = function
    | Anon -> true
    | p -> element = p
  let to_json = function
    | Anon -> `String "*"
    | AWS l -> `O ["AWS", `A (List.rev_map map_string l)]
    | Federated f -> `O ["Federated", `String f]
    | Service s  -> `O ["Service", `String s]
    | CanonicalUser u -> `O ["CanonicalUser", `String u]
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

  let to_json_key key = function
    | [] -> []
    | l -> [ key, arr_or_item (List.rev_map Principal.to_json l) ]

  let to_json key p : (string * Json.t) list =
    List.rev_append (to_json_key key p.incl)
      (to_json_key ("Not" ^ key) p.excl)
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
  let to_string s =
    String.concat ":" ["arn";"aws";"s3";"";""; Neturl.join_path s]
end
module Resources = Elements(Resource)

type statement = {
  sid: string option;
  effect: effect;
  principals: Principals.t;
  actions: Actions.t;
  resources: Resources.t;
  condition: (string * CodedIO.Json.t) list;
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
    condition: (string * CodedIO.Json.t) list;
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
    | "Sid", sid -> { accum with sid = Some (CodedIO.Json.expect_string sid) }
    | "Effect", e -> { accum with effect = Some (parse_effect e) }
    | "Principal", p ->
      { accum with principal = [Principal.of_string_json p] }
    | "NotPrincipal", p ->
      { accum with not_principal = [Principal.of_string_json p] }
    | "Action", a ->
      { accum with
        action = CodedIO.Json.expect_array_or_string a }
    | "NotAction", a ->
      { accum with
        not_action = CodedIO.Json.expect_array_or_string a }
    | "Resource", a ->
      { accum with resource = CodedIO.Json.expect_array_or_string a }
    | "NotResource", a ->
      { accum with not_resource = CodedIO.Json.expect_array_or_string a }
    | "Condition", c ->
      { accum with condition = (CodedIO.Json.expect_obj c) }
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
  valid_statement (List.fold_left Stmt.fold Stmt.empty (CodedIO.Json.expect_obj json))

let fold_toplevel accum = function
  | "Version", v -> { accum with version = CodedIO.Json.expect_string v }
  | "Id", id -> { accum with id = Some (CodedIO.Json.expect_string id) }
  | "Statement", stmt ->
    { accum with
      statements = List.rev_map map_statement (CodedIO.Json.expect_array stmt) }
  | f, _ -> failwith ("Unknown field " ^ f)

let of_string s =
  let json = CodedIO.Json.expect_obj (CodedIO.Json.of_string s) in
  List.fold_left fold_toplevel empty json

let is_anon_bucket_policy p bucket =
  valid p bucket && match p.statements with
  | [ { effect = Allow;
        principals = { Principals.incl = [Principal.Anon]; excl = [] };
        actions = Actions.Incl [ "s3:GetObject" ]; _
      } ]
    -> true
  | _ -> false

let build_anon_policy bucket = {
  version = "2012-10-17"; id = None;
  statements = [{
    sid = None;
    effect = Allow;
    principals = Principals.of_lists ([Principal.Anon], []);
    actions = Actions.of_lists (["s3:GetObject"], []);
    resources = Resources.of_lists ([Printf.sprintf "arn:aws:s3:::%s/*" bucket], []);
    condition = []
    }]
}

let json_of_stmt s =
  `O (("Effect", if s.effect = Allow then `String "Allow" else `String "Deny") ::
      List.rev_append (Principals.to_json "Principal" s.principals)
        (
          Actions.to_json "Action" s.actions ::
          Resources.to_json "Resource" s.resources
          ::
          if s.condition = [] then [] else ["Condition", `O s.condition]
        )
    )

let json_of_policy p : Json.t =
  `O [
    "Version", `String p.version;
    (* TODO: id *)
    "Statement", `A (List.rev_map json_of_stmt p.statements)
  ]
