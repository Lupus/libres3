
open Cohttp_lwt_unix
let service (req, body) =
  (* requires absolute uri or Host header *)
  Client.call ~headers:(Request.headers req) ~body (Request.meth req) (Request.uri req)
