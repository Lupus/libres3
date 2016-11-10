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

(* DELETE Bucket policy
   GET Bucket policy
   PUT Bucket policy *)

open Astring
open Rresult
open Json_encoding

type obj
type bucket
type subresource
type any

module Permission = struct
  type 'a t = string (* TODO *)


  let obj perm = perm
  let bucket perm = perm
  let subresource perm = perm

  let all = "*"

  let encoding : 'a t Json_encoding.encoding = string
end

module GetBucket = struct
  let policy = Permission.subresource "s3:GetBucketPolicy"
end

module DeleteBucket = struct
  let policy = Permission.subresource "s3:DeleteBucketPolicy"
end

module PutBucket = struct
  let policy = Permission.subresource "s3:PutBucketPolicy"
end

module ObjSet = Set.Make(struct type t = obj Permission.t let compare = compare end)
module BucketSet = Set.Make(struct type t = bucket Permission.t let compare = compare end)
module SubresourceSet = Set.Make(struct type t = subresource Permission.t let compare = compare end)

module ARN = struct
  type pattern = Literal of string | Prefix of string | Any

  let is_wildcard c = c = '*' || c = '?'
  
  let pattern_of_string_exn = function
  | str when not (String.exists is_wildcard str) -> Literal str
  | "*" -> Any
  | str when String.is_suffix ~affix:"*" str &&
             String.find is_wildcard str = Some (String.length str - 1) ->
      Prefix (String.drop ~rev:true ~max:1 str)
  | str ->
      failwith "TODO: Arbitrary wildcard matches are not supported yet"

  let pattern_to_string = function
  | Any -> "*"
  | Prefix str -> str ^ "*"
  | Literal str -> str

  let pattern_matches pat str = match pat with
  | Any -> true
  | Literal expect -> String.equal expect str
  | Prefix pre -> String.is_prefix ~affix:pre str

  type t = {
    partition: pattern;
    service: pattern;
    region: pattern;
    namespace: pattern;
    relative_id: pattern
  }

  let of_string_exn str =
    String.cuts ~sep:":" str |> function
    |["arn";partition;service;region;namespace;relative_id] ->
        {partition = pattern_of_string_exn partition;
         service = pattern_of_string_exn service;
         region = pattern_of_string_exn region;
         namespace = pattern_of_string_exn namespace;
         relative_id = pattern_of_string_exn relative_id
        }
    | _ ->
        failwith ("Cannot parse ARN: " ^ str)

  let to_string t =
    String.concat ~sep:":" [
      "arn";
      pattern_to_string t.partition;
      pattern_to_string t.service;
      pattern_to_string t.region;
      pattern_to_string t.namespace;
      pattern_to_string t.relative_id;
    ]

  let is_valid_partition arn =
    (pattern_matches arn.partition "aws" ||
    pattern_matches arn.partition "aws-cn")

end

module Resource = struct
  type t = ARN.t
            
  open ARN

  let is_s3 arn =
    is_valid_partition arn &&
    pattern_matches arn.service "s3" &&
    arn.region = Literal "" &&
    arn.namespace = Literal ""

  let of_string_exn str =
    let arn = of_string_exn str in
    if is_s3 arn then arn
    else failwith ("Not an S3 ARN: " ^ str)

  let get_single_bucket arn =
    if is_s3 arn then
      match arn.relative_id with
      | Any | Literal _ -> None
      | Prefix str ->
          match String.cut ~sep:"/" str with
          | Some (bucket, "") -> Some bucket
          | _ -> None
   else None

  let encoding = conv to_string of_string_exn string
end

module Principal = struct
  type t = AWS of ARN.t option | CanonicalUser of string

  open ARN
  let is_account arn =
    is_valid_partition arn &&
    pattern_matches arn.service "iam" &&
    arn.region = Literal ""

  let arn_of_string str =
    if str = "*" then None
    else
    let arn = ARN.of_string_exn str in
    if is_account arn then Some arn
    else failwith ("Not an Account ARN: " ^ str)

  let string_of_arn = function
  | None -> "*"
  | Some a -> ARN.to_string a

  let to_aws a = AWS a
  let of_aws = function
  | AWS a -> Some a
  | _ -> None

  let to_canon c = CanonicalUser c
  let of_canon = function
  | CanonicalUser c -> Some c
  | _ -> None

  let of_anon = function
  | AWS None -> Some "*"
  | _ -> None

  let to_anon = function
  | "*" -> AWS None
  | _ -> failwith "Not a wildcard"

  let encoding = union [
      case (conv string_of_arn arn_of_string string |> req "AWS" |> obj1) of_aws to_aws;
      case (req "CanonicalUser" string |> obj1) of_canon to_canon;
      case string of_anon to_anon
    ] 

end

module Condition = struct
  module StringOp = struct
    type t = Equals | NotEquals | EqualsIgnoreCase | NotEqualsIgnoreCase | Like | NotLike

    let encoding = string_enum [
        "StringEquals", Equals;
        "StringNotEquals", NotEquals;
        "EqualsIgnoreCase", EqualsIgnoreCase;
        "Like", Like;
        "NotLike", NotLike
      ]

    let equals_nocase a b =
      String.(equal (Ascii.lowercase a) (Ascii.lowercase b))

    let eval op a b = match op with
    | Equals -> String.equal a b
    | NotEquals -> not (String.equal a b)
    | EqualsIgnoreCase -> equals_nocase a b
    | NotEqualsIgnoreCase -> not (equals_nocase a b)
    | Like | NotLike -> failwith "TODO: Like not supported yet"
  end
  module NumericOp = struct
    type t = Equals | NotEquals | LessThan | LessThanEquals | GreaterThan | GreaterThanEquals

    let encoding = string_enum [
        "NumericEquals", Equals;
        "NumericNotEquals", NotEquals;
        "NumericLessThan", LessThan;
        "NumericLessThanEquals", LessThanEquals;
        "NumericGreaterThan", GreaterThan;
        "NumericGreaterThanEquals", GreaterThanEquals
      ]

    let eval op a b =
      let cmp = Int64.compare a b in
      match op with
      | Equals -> cmp = 0
      | NotEquals -> cmp <> 0
      | LessThan -> cmp < 0
      | LessThanEquals -> cmp <= 0
      | GreaterThan -> cmp > 0
      | GreaterThanEquals -> cmp >= 0
  end

  module DateOp = struct
    type t = Equals | NotEquals | LessThan | LessThanEquals | GreaterThan | GreaterThanEquals

    let encoding = string_enum [
        "DateEquals", Equals;
        "DateNotEquals", NotEquals;
        "DateLessThan", LessThan;
        "DateLessThanEquals", LessThanEquals;
        "DateGreaterThan", GreaterThan;
        "DateGreaterThanEquals", GreaterThanEquals
      ]

    open CalendarLib
    let eval op a b =
      let cmp = Calendar.compare a b in
      match op with
      | Equals -> cmp = 0
      | NotEquals -> cmp <> 0
      | LessThan -> cmp < 0
      | LessThanEquals -> cmp <= 0
      | GreaterThan -> cmp > 0
      | GreaterThanEquals -> cmp >= 0
  end

  module BoolOp = struct
    type t = Bool

    let encoding = string_enum ["Bool", Bool]

    let eval Bool (a:bool) (b:bool) = a = b
  end

  type 'a t =
    | Str : StringOp.t * string -> string t
    | Numeric: NumericOp.t * int64 -> int64 t
    | Date : DateOp.t * CalendarLib.Calendar.t -> CalendarLib.Calendar.t t
    | Bool: BoolOp.t * bool -> bool t

  let eval (type a): a t -> a -> bool = fun op b -> match op with
  | Str (op, a) -> StringOp.eval op a b
  | Numeric (op, a) -> NumericOp.eval op a b
  | Date (op, a) -> DateOp.eval op a b
  | Bool (op, a) -> BoolOp.eval op a b
end

module Statement = struct
  type effect = Allow | Deny

  let effect_encoding = string_enum [
      "Allow", Allow;
      "Deny", Deny
    ]

  type 'a element = Pos of 'a | Not of 'a

  type t = {
    sid: string option;
    effect: effect;
    principal: Principal.t element;
    action: any Permission.t list element;
    resource: Resource.t list element;
  }

  let element name = function
  | Some _, Some _ -> failwith (Printf.sprintf "only one of %s or %s is permitted" name name)
  | Some pos, None -> Pos pos
  | None, Some neg -> Not neg
  | None, None -> failwith (Printf.sprintf "either %s or Not%s is required" name name)

  let v (sid, effect, principal, not_principal, action, not_action, resource, not_resource) =
    { sid; effect;
      principal = element "Principal" (principal, not_principal);
      action = element "Action" (action, not_action);
      resource = element "Resource" (resource, not_resource)
    }

  let pos = function Pos a -> Some a | Not _ -> None
  let neg = function Not a -> Some a | Pos _ -> None
  
  let to_v {sid;effect;principal;action;resource;} =
    (sid,effect,(pos principal),(neg principal),(pos action),(neg action),
     (pos resource),(neg resource))

  let encoding = obj8
      (opt "Sid" string)
      (req "Effect" effect_encoding)
      (opt "Principal" Principal.encoding)
      (opt "NotPrincipal" Principal.encoding)
      (opt "Action" (list Permission.encoding))
      (opt "NotAction" (list Permission.encoding))
      (opt "Resource" (list Resource.encoding))
      (opt "NotResource" (list Resource.encoding)) |> conv to_v v

  let pp ppf = failwith "TODO"
end

type t = {
  version: string;
  id: string option;
  statements: Statement.t list;
  (*  condition *)
}

let v (version, id, statements) =
  { version; id; statements }

let to_v t = t.version, t.id, t.statements

let encoding = obj3
    (req "Version" string)
    (opt "Id" string)
    (req "Statement" (list Statement.encoding)) |>
    conv to_v v

let pp ppf t = Fmt.pf ppf "@[version: %s; id: %a; statement: %a@]"
    t.version Fmt.(option string) t.id Fmt.(list Statement.pp) t.statements

let example = "{\"Version\":\"2012-10-17\",\"Statement\":[{\"Sid\":\"ExampleStatement1\",\"Effect\":\"Allow\",\"Principal\":{\"AWS\":\"arn:aws:iam::00:user/John\"},\"Action\":[\"s3:GetBucketLocation\",\"s3:ListBucket\",\"s3:GetObject\"],\"Resource\":[\"arn:aws:s3:::volx1\"]}]}"
(*
   READ: Get*
   WRITE: Put*
   MANAGER: Put*Acl

   store in customVolumeMeta (requires manager)

   check if you have it -> escalate to volume owner
   add libres3-policy-<user> user user to all S3 buckets,
   and escalate to that

   SX ACL mapped to S3 ACL mapped to S3 policy
   flag to specify which one is in effect in customVolumeMeta

   SX ACL needs to be conservative: if you don't grant all s3:list* policies for e.g.
    then user is not granted access at SX level

   try regular op, fetch/cache policy/ apply policy

   bucket owner vs object owner

   already existing volumes
*)
