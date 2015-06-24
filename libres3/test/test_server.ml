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

open OUnit
open CanonRequest

type methods = [ `DELETE | `GET | `HEAD | `POST | `PUT | `UNSUPPORTED ]
type request_test = {
  name: string;
  (* input *)
  headers: (string * string) list;
  req_method: methods;
  orig_url: string;
  (* expected output *)
  expected_tosign: string;
  expected_valid: bool;
  expected_bucket: string;
  expected_path: string;
}

let assert_str_equal ?msg expected actual =
  assert_equal ?msg ~printer:(fun s -> s) expected actual;;

let map_method = function
  | `DELETE | `GET | `HEAD | `UNSUPPORTED as m -> m
  | `POST ->
     `POST ()
  | `PUT ->
      `PUT ()

let validate canon tosign =
  let expected_signature = Cryptoutil.sign_str !Config.secret_access_key tosign in
  match CanonRequest.parse_authorization canon with
  | Authorization (_, signature,_) ->
      signature = expected_signature
  | _ -> false

let test_request_parse_sign data =
  data.name>::(fun () ->
    let meth = map_method data.req_method in
    let canon_req =
      canonicalize_request ~id:(RequestId.generate ()) meth {
        req_headers = data.headers;
        undecoded_url = data.orig_url
      } in
    assert_str_equal ~msg:"bucket" data.expected_bucket (Bucket.to_string canon_req.bucket);
    let tosign = string_to_sign canon_req in
    assert_str_equal ~msg:"string-to-sign" data.expected_tosign tosign;
    let valid = validate canon_req tosign in
    assert_equal ~msg:"signature" data.expected_valid valid;
    assert_str_equal ~msg:"path" data.expected_path canon_req.path;
    );;

let secret_key_v4 = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
let secret_key_v2 = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
let test_request_parse_sign_v4 (key, data,expected_canonical,body) =
  data.name>::(fun () ->
    let meth = map_method data.req_method in
    let canon_req =
      canonicalize_request ~id:(RequestId.generate ()) meth {
        req_headers = data.headers;
        undecoded_url = data.orig_url
      } in
    assert_str_equal ~msg:"bucket" data.expected_bucket (Bucket.to_string canon_req.bucket);
    match CanonRequest.parse_authorization canon_req with
    | AuthorizationV4 (authv4, expected_signature,_) ->
      let sha256 = match body with
        | Some s -> Some (Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) s)
        | None -> None in
      let canonical, tosign = string_to_sign_v4 authv4 ?sha256 ~canon_req in
      assert_str_equal ~msg:"canonical request" expected_canonical canonical;
      assert_str_equal ~msg:"string-to-sign-v4" data.expected_tosign tosign;
      let signature =
        sign_string_v4 ~key authv4.credential tosign
      in
      let valid = signature = expected_signature in
      if data.expected_valid then
        assert_str_equal ~msg:"signature" expected_signature signature
      else
        assert_equal ~msg:"sig should be invalid" data.expected_valid valid;
      assert_str_equal ~msg:"path" data.expected_path canon_req.path;
    | AuthMalformed s ->
      assert_failure ("cannot parse auth header: " ^ s)
    | AuthNone -> assert_failure ("auth = none")
    | AuthEmpty ->  assert_failure "auth empty"
    | AuthDuplicate -> assert_failure "auth duplicate"
    | Authorization (a,_,_) -> assert_failure ("bad auth version: " ^ a)
  );;

let request_data_v4 = [
  secret_key_v4, {
    name = "GET with parameters";
    headers = [
      "Date", "Mon, 09 Sep 2011 23:36:00 GMT";
      "Host", "host.foo.com";
      "Authorization", "AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20110909/us-east-1/host/aws4_request, SignedHeaders=date;host, Signature=be7148d34ebccdc6423b19085378aa0bee970bdc61d144bd1a8c48c33079ab09"
    ];
    req_method = `GET;
    orig_url = "/?foo=Zoo&foo=aha";
    expected_tosign =
      "AWS4-HMAC-SHA256\n20110909T233600Z\n20110909/us-east-1/host/aws4_request\ne25f777ba161a0f1baf778a87faf057187cf5987f17953320e3ca399feb5f00d";
    expected_valid = true;
    expected_bucket = "host.foo.com";
    expected_path = "/";
  }, "GET\n/\nfoo=Zoo&foo=aha\ndate:Mon, 09 Sep 2011 23:36:00 GMT\nhost:host.foo.com\n\ndate;host\ne3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855", Some "" ;
  secret_key_v2,
  {
    name = "pre-signed GET v4 URL";
    headers = [
      "Host", "examplebucket.s3.amazonaws.com"
    ];
    req_method = `GET;
    orig_url =
      "/test.txt?X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIOSFODNN7EXAMPLE%2F20130524%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20130524T000000Z&X-Amz-Expires=86400&X-Amz-SignedHeaders=host&X-Amz-Signature=aeeed9bbccd4d02ee5c0109b86d86835f995330da4c265957d157751f604d404";
    expected_tosign =
      "AWS4-HMAC-SHA256\n20130524T000000Z\n20130524/us-east-1/s3/aws4_request\n3bfa292879f6447bbcda7001decf97f4a54dc650c8942174ae0a9121cf58ad04";
    expected_valid = true;
    expected_bucket = "examplebucket";
    expected_path = "/test.txt"
  },
  "GET\n/test.txt\nX-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Credential=AKIAIOSFODNN7EXAMPLE%2F20130524%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Date=20130524T000000Z&X-Amz-Expires=86400&X-Amz-SignedHeaders=host\nhost:examplebucket.s3.amazonaws.com\n\nhost\nUNSIGNED-PAYLOAD", Some ""
]

let request_data = [
  {
    name = "Object GET";
    headers = [
      "Host","johnsmith.s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 19:36:42 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:bWq2s1WEIj+Ydj0vQ697zp+IXMU="
    ];
    req_method = `GET;
    orig_url = "/photos/puppy.jpg";
    expected_tosign = "GET\n\n\nTue, 27 Mar 2007 19:36:42 +0000\n/johnsmith/photos/puppy.jpg";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/photos/puppy.jpg";
  };
  {
    name = "Object PUT";
    headers = [
      "Content-Type","image/jpeg";
      "Content-Length", "94328";
      "Host","johnsmith.s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 21:15:45 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:MyyxeRY7whkBe+bq8fHCL/2kKUg="
    ];
    req_method = `PUT;
    orig_url = "/photos/puppy.jpg";
    expected_tosign = "PUT\n\nimage/jpeg\nTue, 27 Mar 2007 21:15:45 +0000\n/johnsmith/photos/puppy.jpg";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/photos/puppy.jpg";
  };
  {
    name = "List";
    headers = [
      "User-Agent","Mozilla/5.0";
      "Host","johnsmith.s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 19:42:41 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:htDYFYduRNen8P9ZfE/s9SuKy0U="
    ];
    req_method = `GET;
    orig_url = "/?prefix=photos&max-keys=50&marker=puppy";
    expected_tosign = "GET\n\n\nTue, 27 Mar 2007 19:42:41 +0000\n/johnsmith/";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/"
  };
  {
    name = "Fetch";
    headers = [
      "Host","johnsmith.s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 19:44:46 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:c2WLPFtWHVgbEmeEG93a4cG37dM="
    ];
    req_method = `GET;
    orig_url = "/?acl";
    expected_tosign = "GET\n\n\nTue, 27 Mar 2007 19:44:46 +0000\n/johnsmith/?acl";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/"
  };
  {
    name = "Delete";
    headers = [
      "User-Agent","dotnet";
      "Host","s3.amazonaws.com";
      "Date","Tue, 27 Mar 2007 21:20:27 +0000";
      "x-amz-date","Tue, 27 Mar 2007 21:20:26 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:R4dJ53KECjStyBO5iTBJZ4XVOaI="
    ];
    req_method = `DELETE;
    orig_url = "/johnsmith/photos/puppy.jpg";
    (* Example in REST API signing docs are wrong: there have to be 4 \n not 3
     * *)
    expected_tosign = "DELETE\n\n\n\nx-amz-date:Tue, 27 Mar 2007 21:20:26 +0000\n/johnsmith/photos/puppy.jpg";
    expected_valid = true;
    expected_bucket = "johnsmith";
    expected_path = "/photos/puppy.jpg"
  };
  {
    name = "Upload";
    headers = [
      "User-Agent","curl/7.15.5";
      "Host","static.johnsmith.net:8080";
      "Date","Tue, 27 Mar 2007 21:06:08 +0000";
      "x-amz-acl","public-read";
      "content-type","application/x-download";
      "Content-MD5","4gJE4saaMU4BqNR0kLY+lw==";
      "X-Amz-Meta-ReviewedBy", "joe@johnsmith.net";
      "X-Amz-Meta-ReviewedBy", "jane@johnsmith.net";
      "X-Amz-Meta-FileChecksum", "0x02661779";
      "X-Amz-Meta-ChecksumAlgorithm","crc32";
      "Content-Disposition","attachment; filename=database.dat";
      "Content-Encoding","gzip";
      "Content-Length","5913339";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:ilyl83RwaSoYIEdixDQcA4OnAnc="
    ];
    req_method = `PUT;
    orig_url = "/db-backup.dat.gz";
    expected_tosign = "PUT\n4gJE4saaMU4BqNR0kLY+lw==\napplication/x-download\nTue, 27 Mar 2007 21:06:08 +0000\nx-amz-acl:public-read\nx-amz-meta-checksumalgorithm:crc32\nx-amz-meta-filechecksum:0x02661779\nx-amz-meta-reviewedby:joe@johnsmith.net,jane@johnsmith.net\n/static.johnsmith.net/db-backup.dat.gz";
    expected_valid = true;
    expected_bucket = "static.johnsmith.net";
    expected_path = "/db-backup.dat.gz"
  };
  {
    name = "ListAllMyBuckets";
    headers = [
      "Host","s3.amazonaws.com";
      "Date","Wed, 28 Mar 2007 01:29:59 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:qGdzdERIC03wnaRNKh6OqZehG9s="
    ];
    req_method = `GET;
    orig_url = "/";
    expected_tosign = "GET\n\n\nWed, 28 Mar 2007 01:29:59 +0000\n/";
    expected_valid = true;
    expected_bucket = "";
    expected_path = "/"
  };
  {
    name = "UnicodeKeys";
    headers = [
      "Host","s3.amazonaws.com";
      "Date","Wed, 28 Mar 2007 01:49:49 +0000";
      "Authorization","AWS AKIAIOSFODNN7EXAMPLE:DNEZGsoieTZ92F3bUfSPQcbGmlM="
    ];
    req_method = `GET;
    orig_url = "/dictionary/fran%C3%A7ais/pr%c3%a9f%c3%a8re";
    expected_tosign = "GET\n\n\nWed, 28 Mar 2007 01:49:49 +0000\n/dictionary/fran%C3%A7ais/pr%c3%a9f%c3%a8re";
    expected_valid = true;
    expected_bucket = "dictionary";
    expected_path = "/français/préfère";
  }
]

type sig_v4_test = {
  cred: string;
  signing_key: int array;
  signature : string;
  string_to_sign: string
}

let sig_v4_data = [{
    cred = "keyid/20110909/us-east-1/iam/aws4_request";
    signing_key = [|152; 241; 216; 137; 254; 196; 244; 66; 26; 220; 82; 43; 171;
                    12; 225; 248; 46; 105; 41; 194; 98; 237; 21; 229; 169; 76;
                    144; 239; 209; 227; 176; 231|];
    signature =
      "ced6826de92d2bdeed8f846f0bf508e8559e98e4b0199114b84c54174deb456c";
    string_to_sign="AWS4-HMAC-SHA256\n20110909T233600Z\n20110909/us-east-1/iam/aws4_request\n3511de7e95d28ecd39e9513b642aee07e54f4941150d8df8bf94b328ef7e55e2"
  }]

let ia_of_str s =
  Array.init (String.length s) (fun i -> Char.code s.[i])

module IADiff = OUnitDiff.ListSimpleMake(struct
    type t = int
    let compare = ( - )
    let pp_printer = Format.pp_print_int
    let pp_print_sep = OUnitDiff.pp_comma_separator
  end
  )

let assert_ia_equal ~msg a b =
  IADiff.assert_equal ~msg (Array.to_list a) (Array.to_list b)

let test_deriv_sign_v4 d =
  "signing key test">::(fun () ->
      let credentials = parse_credential d.cred in
      let signing_key = signing_key_v4 secret_key_v4 credentials in
      assert_ia_equal ~msg:"signing key" d.signing_key (ia_of_str signing_key);
      let signature = sign_string_v4 ~key:secret_key_v4 credentials d.string_to_sign
      in
      assert_str_equal ~msg:"signature" d.signature signature)

let suite =
  "Request">::: [
    "signing">:::
      List.map test_request_parse_sign request_data;
    "deriv-sign-v4">:::
      List.map test_deriv_sign_v4 sig_v4_data;
    "canon-signing-v4">:::
      List.map test_request_parse_sign_v4 request_data_v4;
  ]
;;

let _ =
  Config.key_id := "AKIAIOSFODNN7EXAMPLE";
  Config.secret_access_key := secret_key_v2;
  Configfile.base_hostname := "s3.amazonaws.com";
  run_test_tt_main suite
;;
