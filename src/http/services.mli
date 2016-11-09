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

module Generic : sig
  type ('a, 'b) t = 'a -> 'b Boundedio.t
  type ('a, 'b, 'c, 'd) filter = ('a, 'b) t -> ('c, 'd) t
  type ('a, 'b) simple_filter = ('a, 'b) t -> ('a, 'b) t
end

module Http : sig
  type body = Cohttp_lwt_body.t
  type req = Cohttp.Request.t * body
  type resp = Cohttp.Response.t * body
  type t = (req, resp) Generic.t
  type filter = (req, resp) Generic.simple_filter
end
