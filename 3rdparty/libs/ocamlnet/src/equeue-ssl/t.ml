(* #use "src/equeue-ssl/t.ml";; *)
#use "topfind";;
#require "netclient,equeue-ssl";;
Ssl.init();;
let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context;;
let tct = Https_client.https_transport_channel_type ctx;;
let p = new Http_client.pipeline;;
p # configure_transport Http_client.https_cb_id tct;;
let c = Http_client.create_aggressive_cache();;
p # set_connection_cache c;;

let opts = p # get_options in
let opts' = { opts with Http_client.verbose_events = true } in
p # set_options opts';;

Http_client.Debug.enable := true;;

let m1 = new Http_client.get "https://godirepo.camlcity.org/";;
let m2 = new Http_client.get "https://godirepo.camlcity.org/";;
let m3 = new Http_client.get "https://godirepo.camlcity.org/";;
let m4 = new Http_client.get "https://godirepo.camlcity.org/";;

let test m =
  p # add m;
  p # run();
  m # status
;;
