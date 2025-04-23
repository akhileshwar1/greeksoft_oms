(* bin/main.ml *)

open Lwt.Infix
open Greeksoft.Rest_client
open Greeksoft.Config

let username = "DHAN"
let password = "greek@123"

let () =
  let open Lwt_main in
  run (
    login ~username ~password
    >>= fun config ->
    Lwt_io.printf "Login successful!\nSession Token: %s\nUser ID: %d\n"
      config.session_token
      config.user_id
    >>= fun () ->
    get_flag_values config
    >>= fun config ->
    Lwt_io.printf "Flags successful!\nIris ip: %s\nIris port: %d\nheartbeat interval: %d\n"
      config.iris_ip
      config.iris_port
      config.heartbeat_interval
  )
