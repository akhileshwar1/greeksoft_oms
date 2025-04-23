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
    >>= fun () ->
    get_login_info config
    >>= fun config ->
    Lwt_io.printf "GetLoginInfo successful!\nGCID: %d\n" config.gcid
    >>= fun () ->
    jlogin_new config
    >>= fun _ ->
    Lwt_io.printf "JLoginNew successful!\n"
    >>= fun () ->
    let dummy_order =
      {
        Entities.Order.tradingsymbol = "GRASIM";
        exchange = "NSE";
        quantity = 1;
        price = 1.0;
        trigger_price = 0.0;
        side = Entities.Order.Buy;
        order_type = Entities.Order.Limit;
        product = Entities.Order.CNC;
        validity = Entities.Order.DAY;
        strategy_name = Some "STOCK";
      }
    in
    place_order config dummy_order
  )
