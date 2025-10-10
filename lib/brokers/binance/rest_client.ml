open Lwt.Infix
open Cohttp_lwt_unix
open Entities.Config

(* Binance endpoints *)
let api_base = get_env_or_default "BINANCE_API_BASE" "https://api.binance.com"
let rest_api_prefix = api_base ^ "/api/v3"
let ws_base = get_env_or_default "BINANCE_WS_BASE" "wss://stream.binance.com:9443/ws/"

let login ~username ~password =
  Lwt.return
  {
    broker = "binance";
    session_token = "";
    user_id = 0;
    broker_config = Binance {
      api_key = username;
      secret_key = password;
      };
  }

(* helper: build query string from (k,v) list *)
let pct_encode s = Uri.pct_encode s
let build_query params =
  params
  |> List.map (fun (k,v) -> pct_encode k ^ "=" ^ pct_encode v)
  |> String.concat "&"

(* sign with Digestif SHA256 HMAC; opam install digestif *)
let sign_query ~secret ~msg =
  let mac = Digestif.SHA256.hmac_string ~key:secret msg in
  Digestif.SHA256.to_hex mac

(* convert Yojson.Basic.t assoc into (string * string) list *)
let yojson_to_params (body : Yojson.Basic.t) : (string * string) list =
  match body with
  | `Assoc kvs ->
      kvs |> List.fold_left (fun acc (k,v) ->
        let vstr = match v with
          | `String s -> s
          | `Float f -> Printf.sprintf "%.8f" f
          | `Int i -> string_of_int i
          | `Bool b -> if b then "true" else "false"
          | `Null -> ""
          | `Assoc _ | `List _ -> Yojson.Basic.to_string v
        in
        (k, vstr) :: acc
      ) [] |> List.rev
  | _ -> []

(* place_order: signs and sends POST /api/v3/order?{qs}&signature=... *)
let place_order ~headers ~body : string Lwt.t =
  (* credentials: for now read from env; better: pass config into this function *)
  let api_key = match Sys.getenv_opt "BINANCE_API_KEY" with Some v -> v | None -> failwith "BINANCE_API_KEY not set" in
  let secret_key = match Sys.getenv_opt "BINANCE_SECRET" with Some v -> v | None -> failwith "BINANCE_SECRET not set" in

  (* build params from body JSON *)
  let params = yojson_to_params body in
  (* add timestamp *)
  let ts = Int64.to_string (Int64.of_float (Unix.gettimeofday () *. 1000.0)) in
  let params = ("timestamp", ts) :: params in

  (* build query string and sign *)
  let qs = build_query params in
  let signature = sign_query ~secret:secret_key ~msg:qs in
  let signed_qs = qs ^ "&signature=" ^ signature in

  (* Construct URI: POST to /api/v3/order?{signed_qs} and empty body *)
  let uri = Uri.of_string (rest_api_prefix ^ "/order?" ^ signed_qs) in

  (* set X-MBX-APIKEY header *)
  let headers = Cohttp.Header.add headers "X-MBX-APIKEY" api_key in

  Client.post ~headers ~body:`Empty uri
  >>= fun (resp, body_stream) ->
  let code = resp |> Response.status |> Cohttp.Code.code_of_status in
  Cohttp_lwt.Body.to_string body_stream >>= fun body_str ->
  (* Optionally handle non-2xx here by failing Lwt *)
  if code >= 200 && code < 300 then
    Lwt.return body_str
  else
    Lwt.fail_with (Printf.sprintf "Binance.place_order HTTP %d: %s" code body_str)

let sign_query ~secret ~msg =
  (* Digestif.SHA256.hmac_string ~key msg returns a digest value;
     Digestif.SHA256.to_hex converts it to lowercase hex string. *)
  let mac = Digestif.SHA256.hmac_string ~key:secret msg in
  Digestif.SHA256.to_hex mac

let cancel_order ~headers ~symbol ~order_id ~api_key ~secret_key =
  let timestamp = Int64.of_float (Unix.gettimeofday () *. 1000.) |> Int64.to_string in
  (* Construct query parameters *)
  let base_params = [
    ("symbol", symbol);
    ("orderId", order_id);
    ("timestamp", timestamp);
  ] in

  let query_string =
    base_params
    |> List.map (fun (k, v) -> Uri.pct_encode k ^ "=" ^ Uri.pct_encode v)
    |> String.concat "&"
  in

  (* compute signature *)
  let signature = sign_query ~secret:secret_key ~msg:query_string in
  let signed_qs = query_string ^ "&signature=" ^ signature in  (* <--- important *)

  let uri = Uri.of_string (rest_api_prefix ^ "/order?" ^ signed_qs) in

  let headers =
    Cohttp.Header.add headers "X-MBX-APIKEY" api_key
  in

  Client.delete ~headers uri >>= fun (resp, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream >>= fun body_str ->
  let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  Lwt_io.printf "[Binance] Cancel order HTTP %d\n%s\n%!" status body_str >>= fun () ->
  Lwt.return body_str

(* Convert a Binance executionReport JSON -> Entities.Order.t by calling Entities.Order.binance_ws_of_yojson *)
let handle_user_data_message raw_json_str =
  match Yojson.Safe.from_string raw_json_str with
  | exception _ -> Lwt_io.printf "[binance] invalid json from user data stream: %s\n%!" raw_json_str
  | j ->
    let open Yojson.Safe.Util in
    let evt = try j |> member "e" |> to_string with _ -> "" in
    (* Binance sometimes wraps in { "data": { ... }, "stream": ... } when multiplexed; handle that *)
    let payload =
      try
        if j |> member "data" <> `Null then j |> member "data" else j
      with _ -> j
    in
    match evt with
    | "executionReport" ->
        (* parse and broadcast *)
        (try
           let order = Entities.Order.binance_ws_of_yojson payload in
           Ws.Ws_server.broadcast_to_clients (Yojson.Safe.to_string (Entities.Order.to_yojson order))
           >>= fun () ->
           Lwt.return_unit
         with ex ->
           Lwt_io.printf "[binance] parse executionReport error: %s\n%!" (Printexc.to_string ex))
    | "outboundAccountPosition" | "balanceUpdate" | "account" ->
        (* optional: you may want to map account/position events to position updates; for now broadcast raw JSON to clients *)
        Ws.Ws_server.broadcast_to_clients (Yojson.Safe.to_string payload)
        >>= fun () ->
        Lwt.return_unit
    | "" ->
        (* If no "e" field, maybe the top-level is the actual event *)
        begin
        (try
           let maybe_type = payload |> member "e" |> to_string in
           if maybe_type = "executionReport" then
             let order = Entities.Order.binance_ws_of_yojson payload in
             Ws.Ws_server.broadcast_to_clients (Yojson.Safe.to_string (Entities.Order.to_yojson order))
             >>= fun () ->
             Lwt.return_unit
           else
             Lwt.return_unit
         with _ -> Lwt.return_unit)
        end
    | other ->
        (* ignore other events but log *)
        Lwt_io.printf "[binance] user-data event: %s\n%!" other

let raw_message_handler (msg : string) : unit Lwt.t =
  (* Called by your connector for each websocket text message *)
  handle_user_data_message msg

(* Helper: create a listenKey for user data stream (Binance legacy flow).
   Requires only X-MBX-APIKEY header (no signature). *)
let get_listen_key ~api_key =
  let uri = Uri.of_string (rest_api_prefix ^ "/userDataStream") in
  let headers = Cohttp.Header.init_with "X-MBX-APIKEY" api_key in
  Client.post ~headers ~body:`Empty uri
  >>= fun (resp, body) ->
  let code = resp |> Response.status |> Cohttp.Code.code_of_status in
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  if code >= 200 && code < 300 then (
    try
      let j = Yojson.Safe.from_string body_str in
      let open Yojson.Safe.Util in
      let listenKey = j |> member "listenKey" |> to_string in
      Lwt.return_ok listenKey
    with _ ->
      Lwt.return_error ("bad-listenKey-json: " ^ body_str)
  ) else Lwt.return_error (Printf.sprintf "listenKey HTTP %d: %s" code body_str)

let connect_to_ws config =
  let api_key, _secret_key =
    match config.broker_config with
    | Binance d -> d.api_key, d.secret_key
    | _ -> failwith "unsupported broker"
  in
  (* Request a listenKey then connect to ws_base + listenKey *)
  get_listen_key ~api_key >>= function
  | Error e ->
      Lwt_io.printf "[binance] failed to get listenKey: %s\n%!" e >>= fun () -> Lwt.return config
  | Ok listen_key ->
      let connect_url = ws_base ^ listen_key in
      let login_msg = `Assoc [] |> Yojson.Safe.to_string in
      let heartbeat_msg = `Assoc [] |> Yojson.Safe.to_string in
      let _ = Ws.Connector.connect_to_data_stream connect_url raw_message_handler login_msg heartbeat_msg in
      Lwt.return config
