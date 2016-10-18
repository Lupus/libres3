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
open Sx_types

module ListNodes = struct
  (* Cluster node list should be of different type than volume locate nodelist,
     so do not just alias to ipaddr.t list *)
  type t = {
    node_list: Ipaddr.t list;
  }

  let v node_list = {node_list}
  let of_v {node_list} = node_list

  let encoding =
    list ipaddr |> req "nodeList" |> obj1 |> obj_opt |>
    conv of_v v

  let get = Uri.make ~query:["nodeList", []] ()

  let target = Cluster

  let example = "{\"nodeList\":[\"127.0.0.2\",\"127.0.0.1\",\"127.0.0.4\",\"127.0.0.3\"]}"

  let pp = Fmt.(list Ipaddr.pp_hum |> using of_v)
end

module Meta = struct
  (* Cluster Meta should be of different type than Volume meta *)
  module T = struct
    type t = {
      cluster_meta: Meta.t
    }

    let v cluster_meta = {cluster_meta}
    let of_v {cluster_meta} = cluster_meta

    let encoding = req "clusterMeta" Meta.encoding |> obj1 |> obj_opt |>
                  conv of_v v

    let target = Cluster

    let example = "{\"clusterMeta\":{\"key1\":\"aabbcc\"}}"

    let pp = Fmt.using of_v Meta.pp
  end
  include T

  module Get = struct
    include T
    let get = Uri.make ~query:["clusterMeta", []] ()
  end

  module Set = struct
    include T
    let put = Uri.make ~path:"/.clusterMeta" ()
  end
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

module Users = struct
  module Key = struct
    type t = Key of Hex.t
    let v k = Key k
    let of_v (Key k) = k
    let encoding = conv of_v v hex_encoding
    let pp ppf (Key h) =
      (* TODO: base64 instead? *)
      Fmt.string ppf (Hex.hexdump_s h)
  end
  type attr = {
    admin: bool;
    user_quota: Int53.t option;
    user_quota_used: Int53.t option;
    user_desc : string option;
  }
  let v (admin,user_quota,user_quota_used,user_desc) =
    {admin;user_quota;user_quota_used;user_desc}
  let of_v {admin;user_quota;user_quota_used;user_desc} =
    admin,user_quota,user_quota_used,user_desc

  let attr_encoding = obj4 (req "admin" bool)
      (opt "userQuota" Int53.encoding) (opt "userQuotaUsed" Int53.encoding)
      (opt "userDesc" string) |> obj_opt |> conv of_v v

  let pp_attr ppf a =
    Fmt.pf ppf "@[admin: %b; quota: %a; quotaUsed: %a; desc: %a@]"
      a.admin (Fmt.option Int53.pp) a.user_quota
      (Fmt.option Int53.pp) a.user_quota_used Fmt.(option string) a.user_desc

  module T = struct
    type t = (User.t * attr) list
  end
  include T
  module List = struct
    include T

    let from_user lst =
      List.rev_map (fun (k,v) -> User.of_v k,v) lst

    let to_user lst =
      List.rev_map (fun (k,v) -> User.v k,v) lst

    let encoding = assoc attr_encoding |> conv from_user to_user
    let pp = Fmt.(pair User.pp pp_attr |> list)

    let target = Cluster
    let example = "{\"admin\":{\"admin\":true},\"testuser\":{\"admin\":false}}"

    let get ?(desc=false) ?(quota=false) ?clones =
      let query =
        query_opt_bool desc "desc" [] |>
        query_opt_bool quota "quota" |>
        query_opt clones (fun name -> ["clones",[name]]) in
      Uri.make ~path:"/.users" ~query
  end
  module Create = struct
    type t = {
      user_name: User.t;
      user_key: Key.t;
      attr: attr;
      existing_name: User.t option;
    }

    let of_v {user_name;user_key;attr;existing_name} =
      user_name,attr.admin,user_key,attr.user_quota,attr.user_desc,existing_name

    let v (user_name,admin,user_key,user_quota,user_desc,existing_name) =
      { user_name; user_key; attr = {
            admin;user_quota;user_desc;user_quota_used=None
          }; existing_name }

    let of_is_admin = function
    | true -> "admin"
    | false -> "normal"

    let to_is_admin_exn = function
    | "admin" -> true
    | "normal" -> false
    | s -> invalid_arg ("Uknown user type " ^ s)

    let user_type_encoding =
      conv of_is_admin to_is_admin_exn string

    let encoding = obj6 (req "userName" User.encoding)
        (req "userType" user_type_encoding)
        (req "userKey" Key.encoding)
        (opt "userQuota" Int53.encoding)
        (opt "userDesc" string)
        (opt "existingName" User.encoding) |> obj_opt |> conv of_v v

    let pp _ = failwith "TODO"
    let example = "{\"userName\":\"newuser\",\"userType\":\"normal\",\"userKey\":\"990f5e344bc83b70de5abed144be7052748c994c\",\"userDesc\":\"some guy\"}"

    let target = Cluster
    let put = Uri.make ~path:"/.users" ()
  end

  module Self = struct
    type t = User.t * attr
    let encoding = singleton User.of_v User.v attr_encoding
    let pp = Fmt.pair User.pp pp_attr
    let example = "{\"testuser\":{\"admin\":false,\"userQuota\":0,\"userQuotaUsed\":0,\"userDesc\":\"Test user description\"}}"
    let get = Uri.make ~query:["self",[]] ()
    let target = Cluster
  end

  module Modify = struct
    type t = {
      user_key: Key.t;
      quota: Int53.t option;
      desc: string option;
    }

    let of_v {user_key;quota;desc} = user_key,quota,desc
    let v (user_key,quota,desc) = {user_key;quota;desc}

    let encoding = obj3 (req "userKey" Key.encoding)
        (opt "quota" Int53.encoding)
        (opt "desc" string) |> obj_opt |> conv of_v v

    let example = "{\"userKey\":\"2507e0c3edcfd4280800db97023b57093c4eac4b\",\"quota\":1073741824}"

    let put = User.uri

    let target = Cluster

    let pp _ = failwith "TODO"
  end

  module Remove = struct
    let target = Cluster
    let delete ?(all_clones=false) u =
      let all = if all_clones then ["all",[]] else [] in
      Uri.with_query (User.uri u) all
  end
end
