(* src/rest_client.ml *)

open Lwt.Infix
open Cohttp_lwt_unix
open Entities.Config

let api_url = get_env_or_default "API_URL" "https://api.kite.trade/orders"
let ws_url = get_env_or_default "WS_URL" "wss://ws.kite.trade?"

let login ~username ~password =
  Lwt.return
  {
    broker = "zerodha";
    session_token = "";
    user_id = 0;
    broker_config = Zerodha {
      api_key = username;
      access_token = password;
      };
  }

let place_order ~headers ~body =
  let uri = Uri.of_string (api_url ^ "/regular") in
  let wrapped_body =
    `Assoc [
      ("request", `Assoc [
        ("data", body);
      ])
    ]
    |> Yojson.Basic.to_string
    |> Cohttp_lwt.Body.of_string
  in
  Client.post ~headers ~body:wrapped_body uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >>= fun body_str ->
  Lwt_io.printf "Order Entry HTTP Response: %s\n" body_str
  >>= fun () ->
  Lwt.return body_str

let raw_message_handler (msg : string) : unit Lwt.t =
  match Yojson.Safe.from_string msg with
  | exception _ -> Lwt_io.printf "Invalid JSON from Iris: %s\n%!" msg
  | json ->
    (* Printf.printf "JSON from Iris: %s\n%!" (Yojson.Safe.to_string json); *)
    let open Yojson.Safe.Util in
    let data = json |> member "response" |> member "data" in
    Printf.printf " data is %s\n%!" (Yojson.Safe.pretty_to_string data);
    begin match Entities.Order.zerodha_ws_of_yojson data with
      | order ->
        Ws.Ws_server.broadcast_to_clients (Yojson.Safe.to_string (Entities.Order.to_yojson order))
      end

let connect_to_ws config = 
  let api_key, access_token =
    match config.broker_config with
    | Zerodha d -> d.api_key, d.access_token
    | _ -> failwith "unsupported broker"
  in
  (* Construct login message *)
  let login_json = `Assoc [] in
  let login_msg= Yojson.Basic.to_string login_json in

  let heartbeat_msg = `Assoc []
  |> Yojson.Basic.to_string
  in

  let params = Printf.sprintf "api_key=%s&access_token=%s" api_key access_token in
  let connect_url = ws_url ^ params in

  (* Call the general connector *)
  let _ = Ws.Connector.connect_to_data_stream connect_url raw_message_handler login_msg heartbeat_msg in
  Lwt.return config
