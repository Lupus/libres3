(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2014 Skylable Ltd. <info-copyright@skylable.com>   *)
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
open CodedIO
let id x = x

let test_roundtrip (input:Xml.t) () =
  assert_equal
    ~printer:Xml.to_string
    input
    (Xml.parse_string (Xml.to_string input));;

let codedio_tests =
  "CodedIO">:::[
    "xml data">::(fun () ->
      assert_equal ~printer:id
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\ntest"
        (Xml.to_string (Xml.d "test"))
    );
    "roundtrip">::(
      test_roundtrip (Xml.tag "ab" [
        Xml.tag "a" ~attrs:[Xml.attr "bar" "4<>"] [];
        Xml.tag "bar" [Xml.d "bar"];
        Xml.tag "x" ~attrs:[
          Xml.attr "x" "4\"";
          Xml.attr ~ns:(Xmlm.ns_xml) "p" "p"
        ] [
          Xml.d "aöpőÁ"
        ]
      ])
    )
  ];;

open Cryptoutil
let cryptoutil_tests =
  "cryptoutil">::: [
    "base64">:::(
      List.map (fun (sin,sout) ->
        sin>::(fun () ->
          assert_equal ~msg:sin ~printer:id sout (base64_encode sin)
        )
      ) [
        "","";
        "f","Zg==";
        "fo","Zm8=";
        "foo","Zm9v";
        "foob","Zm9vYg==";
        "fooba","Zm9vYmE=";
        "foobar","Zm9vYmFy"
      ]);
      "hmac-sha1">:::(
        List.map (fun (key,buf,out) ->
        buf>::(fun () ->
          let b = Buffer.create 16 in
          Buffer.add_string b buf;
          assert_equal ~msg:buf ~printer:Digest.to_hex
            out (hmac_sha1 key b)
        )
      ) [
        "\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b\x0b",
        "Hi There",
        "\xb6\x17\x31\x86\x55\x05\x72\x64\xe2\x8b\xc0\xb6\xfb\x37\x8c\x8e\xf1\x46\xbe\x00"
      ]
      )
  ];;

let murmur_test (input,seed,expected) =
  input>::(fun () ->
    let result =
      (Murmur.murmurhash64b input seed) in
    assert_equal ~msg:"Murmur hash value mismatch" ~printer:Int64.to_string result expected
  )

let gen_murmur_tests m =
  Array.to_list (Array.mapi (fun n expected ->
    let s = String.make n '\x00' in
    for i = 0 to n-1 do
      s.[i] <- Char.chr i
    done;
    s, 0x1337l, expected
  ) m)


let murmur_tests =
  "Murmur">::: List.rev_map murmur_test (gen_murmur_tests [|
      0xb371d410444236ffL; 0x90d4b955085b6328L; 0xe877ccaa51496d77L; 0x3f75336f2acce5ddL;
      0xcee0255754ace6cbL; 0x589b89aa9daa9917L; 0x97678c32390004b9L; 0x2414ac70ac80b8e9L;
      0x3d8e7c113187b02eL; 0xa4b7e7343a6af6e8L; 0x6a736bc19a876be9L; 0xb2176045eace8162L;
      0x495ec26f9c8fa47aL; 0x859f929f514c5187L; 0x5bbdb8f4c4075cc9L; 0xb8a87e90ca4e7173L;
      0xc2545138397bbf68L; 0x6c26014c42804f6eL; 0x47289f0f0cc9b989L; 0xf2f4f752876c294eL;
      0x2024fd49eb45b25aL; 0x0cc2cb224d7963aeL; 0x827d6f2aeabb9a5aL; 0xfe036fbfdad88d4aL;
      0x513acd025c9eb971L; 0x4e14352f7254acb0L; 0xa8e8d16e6f0e7a56L; 0x201dd31aa54ffe1eL;
      0x6f801cbef1ada2adL; 0x62503eae78bab6ccL; 0xf0b6b6c7299d2a62L; 0x9aed1e9313758861L;
      0x1867192fd15ea38cL; 0xd0ccd5b24b8f31feL; 0xe7c3d23ca705feb5L; 0x9bdc365203ebb875L;
      0xe16d14124745ffd4L; 0xf96470c39a3e1114L; 0x664b59e2f9b96385L; 0x9c59d5b88c39d2b1L;
      0x7f10b049242ac7b3L; 0x1dab47383a754f33L; 0x27ec0279e3f551bcL; 0xf65857300aee6943L;
      0xbc40209b2aa056f2L; 0x71b5d185e9192dcdL; 0x3a622b57b27e1caeL; 0xbe45fc6af0ec248aL;
      0xcf8eb8bb7aa5c015L; 0x4857e1b4097417e7L; 0x089511722b596db3L; 0xf35f31f406da045aL;
      0x99257fbe1b88cb06L; 0x9c7a88c292f0f538L; 0xb2426918afaf626dL; 0x9bb9be1e53234dc2L;
      0x0780f6642d9ecdc1L; 0x9a0fce09a727c8b3L; 0x6e872f156ce80a3aL; 0x0c78e073f4925a5eL;
      0x4151b9dca2703a0aL; 0x903339192d862cb6L; 0x996761aef298c982L; 0x6cb5057d7403900bL;
      0x7e64ca7ad0dcddd4L; 0x4ce97dffbe675759L; 0xb180829b535b2895L; 0xd3276f0eb2f403b1L;
      0x3b8b86ad3ce5e9eaL; 0xae5994a841b5a7e6L; 0x87eae656cac9ea86L; 0x4d52f5eb2d940ee4L;
      0xc2a257d883a00ce5L; 0x587cfcc3e90d176eL; 0x6f2f19f5de69e142L; 0xdb467ca3fc1e227eL;
      0xa5734db0c52b235eL; 0x285901b3eb6e76bbL; 0x6f4b74c7f91fcf7eL; 0x7d71b34ed8313441L;
      0x48ccece42cae78f2L; 0x7bf4e517504acd00L; 0x842a590b23d189a5L; 0x6f914947561f5871L;
      0x071bde7393b020baL; 0xfd505636bf2af955L; 0x2579d26e1b76c815L; 0x51cbae2a1e0ceb24L;
      0xd078f013ae39d9abL; 0x50ac289dd24a3ca1L; 0x7a9253c61b679e42L; 0xe28aea04526a01e6L;
      0xc7f49ff631058dc6L; 0xa4c5318dcd0fcf73L; 0x37d7d78d3b5a087dL; 0x041515087b2308dbL;
      0xa2583db7f9302cedL; 0xae0954b2cc578993L; 0x699c8bad49ea302dL; 0xa0d02b5d6e49388bL;
      0x8dc2d4e305f6078dL; 0x786c2939cf94f9bdL; 0xf4bd089608d2b016L; 0x621d158e2945bf24L;
      0x76fd36d271d0df28L; 0xf29cf80e8ca5f5ffL; 0x4093da56ddfe713eL; 0x535c24325a32684eL;
      0xbdf4f54ecd35624cL; 0xc8b69c99d45d9eceL; 0x28a5a35a2b67f5e0L; 0xd2f088ad35556deaL;
      0x2062461e571181aaL; 0x7068d0a415082165L; 0x745279793cfdb88aL; 0xf8e06266112cc420L;
      0xa025dbd6ff62daf7L; 0x0cc83baefd29a7b1L; 0xabcca564a1866d26L; 0x31a72162f81e25a3L;
      0x65a9b9dc236f41ebL; 0xd73a855290fe2478L; 0xdc4ec4f9b5a523ddL; 0x92880051b78c04c0L;
      0xdc0aa440c637f91bL; 0xe79de69667dc0d78L; 0xbed490d7e515f37bL; 0x3c1d6886be333687L;
      0x2fccbcb44509f16cL; 0x049f9c7765f71b28L; 0xdeea289d2f9aecb1L; 0xc235382b8301689eL;
      0x118405aa6a24b63cL; 0xec46e8226dad3a11L; 0x74c28a5dae91ef4eL; 0xae1f8afb7f38f623L;
      0x83ba881d966bef5fL; 0xdd615610ee5a13a5L; 0xaa29be07144aa191L; 0x41c86f5fd47214b6L;
      0xb23d059e48ccc300L; 0xf9bddeee6098a7f9L; 0x604ec20d9b107bb8L; 0xc68e5a6cfe682132L;
      0x9e42de54b4cfc545L; 0x794a9571f13a4273L; 0x753827ebdc120613L; 0xefb2fbed795cb9b0L;
      0xeba487dc23d9f1b1L; 0x39a1555f063a92f1L; 0x0927e343d1183532L; 0x488d61f644c5ed78L;
      0x5596a9f34ed4d091L; 0x1d411830ecc531a1L; 0x149ffc091d906b13L; 0x669e58b77ca6e0c6L;
      0x53635e2e2c993ae1L; 0x059de11a98065fe2L; 0x8f62539170dd2ac2L; 0x10ca717c621d5d6fL;
      0x3b6860addbffda7cL; 0x9c4cb55f6183a11fL; 0xbf341bae5f95bd29L; 0x270c248a69c9e4e4L;
      0xdfc9205ca697efb9L; 0x86eb6ead42f77a85L; 0x849fa4805ffbbfafL; 0x40feb1b604ff7560L;
      0xf7f658f7670f69ecL; 0x9d8a4c331deba636L; 0xace4e3f4825627b9L; 0x4d890196b852524bL;
      0x8f5817f021345b7fL; 0xe5d4436db2ba08a5L; 0x341a161a9b282d6dL; 0x50c136e2a0f49e09L;
      0x76a17aed0542ef1aL; 0x8501e80671ae2af2L; 0x8fadd6d38db42945L; 0x406aed89e9ceff01L;
      0xf2affef849f13952L; 0x74d334c0f6d21ec3L; 0x3446d9e81931775cL; 0x4f151cd65e94ca6dL;
      0x436323a0dedc152aL; 0x20b1288f41e9e677L; 0x773d1312ba5dd12cL; 0xfe6ef8a36f133a3aL;
      0x512472b56650d99eL; 0x3b2401393121dfbcL; 0xa3e6b52d01158105L; 0x5d01686126586d43L;
      0xf554ccc478266ce0L; 0x057053e35ba7c857L; 0xf85847f2c862f1c6L; 0x717a739b52820aa6L;
      0xe1cad04de7635264L; 0x4a1849fa4dfea8c2L; 0x2b66b5fe97b654d2L; 0xd17c81f797ed5514L;
      0x87aefa09acdbd75bL; 0xcbb34f7ba3bcd2c2L; 0x9d8cfe0cb7919117L; 0x6ff940c0a0251a17L;
      0xbc46dcea328fa3e8L; 0x4f67b35f06693dd0L; 0xe235576cafd02811L; 0xf12d28407a8c233bL;
      0xae91636a9882bb38L; 0x77ee8c9589329907L; 0x26b231473701b3ddL; 0xeb4b24b75da2ff78L;
      0x7c16382e7383fd62L; 0xd236d0a5787e884aL; 0x4d0dd606fdc86565L; 0xc903be301e22c84bL;
      0x53e78f0c7da1185cL; 0x181b83237a2f45aeL; 0x3c70bd90de0a948dL; 0x45df342893917a03L;
      0x68231095d3059cceL; 0x5c89d13db929ffb5L; 0x2f34836f45eb7f49L; 0x33f4aa68a2bb31f2L;
      0x8e9760ff14ea0af9L; 0x16925be3bdf0474aL; 0x0654009af91c3f46L; 0x2c8bc5779b8f23aaL;
      0x0683a1f667a2cffaL; 0x88349056a2fbfff0L; 0xe65def20387f7a3aL; 0x5ea9ba38f22f48bdL;
      0xa206a06f8b227a11L; 0x08f554efad3b0cdcL; 0x245f59dedee84f6bL; 0x8e556cb249174a01L;
      0xefe65d2d3fcde5d3L; 0x55cbe68a3b751281L; 0x6a869d560c7d1d41L; 0xed8f6fa5d6fd33d1L;
      0x28a1ffd0f77d24f6L; 0xe4ad253cbee7c886L; 0x86c4ebc7d2c4167bL; 0xbdcb6224fee67e2bL;
      0x6fc0e423f0ab911fL; 0x5a335bd0ec8a9491L; 0x6c7a4f9d750b061bL; 0x7aaad9de1348bca6L;
      0xf10824332424d288L; 0x260cd0e01d8a3db4L; 0x3f2bfa502e2bc5c8L; 0xeceebfc9d919f717L;
      0xa4aae17d6cc96b12L; 0x7f1b80adf48d4e15L; 0xdc623d2656112a06L; 0x5a3ea7e7f69c7da1L;
      0xe29bfcc26ddd1da9L;
    |])

let suite =
  "Misc">:::[
    codedio_tests;
    cryptoutil_tests;
    murmur_tests
  ]

let _ =
  run_test_tt_main suite;;
