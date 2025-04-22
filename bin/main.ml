(* bin/main.ml *)

open Lwt.Infix
open Greeksoft_oms

let username = "31500013A"
let password = "greek@123"

let () =
  let open Lwt_main in
  run (
    Rest_client.login ~username ~password
    >>= fun config ->
    Lwt_io.printf "Login successful!\nSession Token: %s\nUser ID: %s\n"
      config.Config.session_token
      config.Config.user_id
  )

