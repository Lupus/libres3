(**************************************************************************)
(*  Copyright (C) 2012-2016, Skylable Ltd. <info-copyright@skylable.com>  *)
(*                                                                        *)
(*  Permission to use, copy, modify, and distribute this software for     *)
(*  any purpose with or without fee is hereby granted, provided that the  *)
(*  above copyright notice and this permission notice appear in all       *)
(*  copies.                                                               *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL         *)
(*  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED         *)
(*  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE      *)
(*  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL  *)
(*  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA    *)
(*  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER     *)
(*  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR      *)
(*  PERFORMANCE OF THIS SOFTWARE.                                         *)
(**************************************************************************)

open OUnit2
open Sexplib
open Sexplib.Std

module SexpDiff = struct
  type t = Sexp.t
  let pp_printer = Sexp.pp_hum
  let compare = Pervasives.compare
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module SexpListDiff = OUnitDiff.ListSimpleMake(SexpDiff)

module SexpableDiff(E:sig
    type t
    val of_string : string -> t
    val sexp_of_t : t -> Sexp.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end) = struct
    type t = E.t
    let printer e = Sexp.to_string_hum (E.sexp_of_t e)

    let pp_diff_sexp ppf = function
      | Sexp.List l1, Sexp.List l2 ->
        SexpListDiff.pp_diff ppf (l1, l2)
      | a, b ->
        Format.fprintf ppf "(%a <> %a)" Sexp.pp_hum a Sexp.pp_hum b

    let pp_diff fmt (a,b) =
      pp_diff_sexp fmt (E.sexp_of_t a, E.sexp_of_t b)

    let assert_equal ?msg ~expected ~real () =
      assert_equal ~msg:"compare and equal" (E.compare expected real = 0) (E.equal expected real);
      assert_equal ~cmp:E.equal ~printer ~pp_diff expected real
end

module UriDiff = SexpableDiff(Uri)

let pairwise f l () =
  List.iter (fun a -> List.iter (fun b -> f a b) l) l

(* OUnit tests *)
let assert_uri_equivalent expected real =
  let expected = Uri.canonicalize (Uri.of_string expected)
  and real = Uri.canonicalize (Uri.of_string real) in
  UriDiff.assert_equal ~expected ~real ()

(* QCheck tests *)

let qcheck_group ?(n=1000) ?size group gen sexp_of_t lst =
  let pp v = "-- " ^ (Sexplib.Sexp.to_string_hum (sexp_of_t v))
  in
  group, lst |> List.rev_map (fun (name, prop) ->
      let run () =
        let t = QCheck.mk_test ~n ~pp ~name ?size gen prop in
        OUnit2.assert_bool name (QCheck.run ~verbose:false t) in
      name, `Quick, run)

open Cohttp
open QCheck.Prop

open QCheck

module GenHeaders = struct
  open Arbitrary
  let gen_header_values lst =
    among lst

  let gen_delta v =
    int 172800 >|= (fun delta -> v ^ "=" ^ (string_of_int delta))

  let list_concat generators =
    list generators >>= fun lst ->
    among [",";", ";" , "] >|= fun sep ->
    String.concat sep lst

  let gen_header =
    pair (return "Cache-Control")
      (list_concat (among ["no-store";"no-cache";"no-transform";"only-if-cached";"must-revalidate";"public";"private";"proxy-revalidate"] |||
                    (among ["max-age";"min-fresh";"s-maxage"] >>= gen_delta)))

  let gen_headers =
    QCheck.Arbitrary.list gen_header >|= Header.of_list

  let gen_meth =
    among [`GET; `HEAD; `POST; `PUT; `DELETE; `TRACE; `OPTIONS]

  let gen_uri =
    lift2 (fun path query -> Uri.make ~path ~query ()) string (list (pair string (list string)))

  let gen_req =
    lift3 (fun headers meth uri -> Request.make ~headers ~meth uri) gen_headers gen_meth gen_uri

  let gen_cache_req =
    gen_req >|= (fun req -> HttpCacheRules.cache_req req)

  let gen_status =
    among [`OK; `Not_modified; `Not_found; `Internal_server_error]

  let gen_resp =
    lift2 (fun status headers -> Response.make ~status ~headers ()) gen_status gen_headers

  let gen_cache_resp ~has_auth uri =
    let now = Unix.gettimeofday () in
    gen_resp >|= (fun resp -> HttpCacheRules.cached_response ~has_auth ~request_time:now uri resp)

  let gen_cache_req_resp =
    gen_cache_req >>= fun req ->
    let has_auth = Header.get_authorization req.headers <> None in
    gen_cache_resp ~has_auth (req.HttpCacheRules.uri) >|= fun resp ->
    (req, resp)

  let sexp_of_cache_req_resp =
    Sexplib.Conv.sexp_of_pair HttpCacheRules.sexp_of_cache_req HttpCacheRules.sexp_of_cached_response

  let check_can_store (req, resp) =
    HttpCacheRules.can_store req resp.HttpCacheRules.cache_status resp.HttpCacheRules.cache_headers

  let has_cache_control directive (req, _) =
    let h = req.HttpCacheRules.headers in
    Header.get_multi h "cache-control" |>
    List.exists (fun v ->
        Stringext.find_from v ~pattern:directive <> None
      )
end

(*
let sexp_of_raw_backtrace_slot slot =
  let open Printexc in
  sexp_of_option (fun loc ->
      Sexp.List [
        sexp_of_bool (Slot.is_raise slot);
        Sexp.List [
          sexp_of_string loc.filename;
          sexp_of_int loc.line_number;
          Conv.sexp_of_pair sexp_of_int sexp_of_int (loc.start_char, loc.end_char)
        ]
      ]) (Slot.location slot)
   *)

let sexp_of_raw_backtrace bt =
  (* sexp_of_option (sexp_of_array sexp_of_raw_backtrace_slot) (Printexc.backtrace_slots bt) *)
  Printexc.raw_backtrace_to_string bt |> sexp_of_string

let () =
  Printexc.record_backtrace true;
  (* sanity tests *)
  Alcotest.run "http" [
    "equivalence", [
      "rfc7230#section-2.7.3", `Quick, pairwise assert_uri_equivalent [
        "http://example.com:80/~smith/home.html";
        "http://EXAMPLE.com/%7Esmith/home.html";
        "http://EXAMPLE.com:/%7esmith/home.html"
      ]
    ];
    (let open GenHeaders in
    qcheck_group "cache" gen_cache_req_resp sexp_of_cache_req_resp [
      "no-store", (has_cache_control "no-store") ==> (!!! check_can_store)
    ]);
  ]
