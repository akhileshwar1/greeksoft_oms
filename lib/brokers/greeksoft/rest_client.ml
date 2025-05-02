(* src/rest_client.ml *)

open Lwt.Infix
open Cohttp_lwt_unix
open Entities.Config

let auth_url = "http://greekapi.greeksoft.in:3001"
let api_url = "http://restapi.greeksoft.in:3333"
let find_string_opt key json =
    match Yojson.Basic.Util.member key json with
    | `Null -> None
    | value -> (try Some (Yojson.Basic.Util.to_string value) with _ -> None)

let find_int_opt key json =
  match Yojson.Basic.Util.member key json with
  | `Null -> None
  | value -> (try Some (Yojson.Basic.Util.to_int value) with _ -> None)

let login ~username ~password =
  let uri = Uri.of_string (auth_url ^ "/auth/greek/sessiontoken") in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let body_json = `Assoc [
    ("username", `String username);
    ("password", `String password);
    ("validFor", `String "30d")
  ] in
  let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string body_json) in

  Client.post ~headers ~body uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >>= fun body_str ->
  Lwt_io.printf "HTTP response bodyy: %s\n" body_str
  >>= fun () ->
  let json = Yojson.Basic.from_string body_str in

  let session_token_opt = find_string_opt "sessionToken" json in
  let user_id_opt = find_int_opt "id" json in

  match session_token_opt, user_id_opt with
  | Some session_token_opt, Some user_id_opt ->
    Lwt.return (with_session empty ~session_token:session_token_opt ~user_id:user_id_opt)
  | _ ->
    failwith "Failed to extract session_token or user_id from login response"


let get_flag_values config =
  let uri = Uri.of_string (api_url ^ "/getFlagValues") in
  let headers =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
    |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
  in
  let body_json =
    `Assoc [
      ("request", `Assoc [
        ("svcVersion", `String "1.0.0");
        ("svcGroup", `String "");
        ("svcName", `String "getFlagValues");
        ("gscid", `String "");
        ("assetType", `String "");
        ("data", `Assoc [])
      ])
    ]
  in
  let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string body_json) in

  Client.post ~headers ~body uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >>= fun body_str ->
  Lwt_io.printf "HTTP response body: %s\n" body_str
  >>= fun () ->
  let json = Yojson.Basic.from_string body_str in
  let data =
    json
    |> Yojson.Basic.Util.member "response"
    |> Yojson.Basic.Util.member "data" in

  let iris_ip_opt = find_string_opt "Iris_IP" data in
  let iris_port_opt = find_int_opt "Iris_Port" data in
  let heartbeat_interval_opt = find_int_opt "heartbeat_Intervals" data in

  match iris_ip_opt, iris_port_opt, heartbeat_interval_opt with
  | Some iris_ip, Some iris_port, Some heartbeat_interval ->
    Lwt.return (with_iris config ~iris_ip ~iris_port ~heartbeat_interval)
  | _ ->
    failwith "Failed to extract iris_ip or iris_port from getFlagValues response"


let get_login_info config =
  let uri = Uri.of_string (api_url ^ "/getLoginInfo") in
  let headers =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
    |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
  in
  let gscid=
    match config.broker_config with
    | Greeksoft g -> g.gscid
  in
  let body_json =
    `Assoc [
      ("request", `Assoc [
        ("svcVersion", `String "1.0.0");
        ("svcGroup", `String "Login");
        ("svcName", `String "getLoginInfo");
        ("data", `Assoc [
          ("gscid", `String gscid);
        ])
      ])
    ]
  in
  let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string body_json) in

  Client.post ~headers ~body uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >>= fun body_str ->
  Lwt_io.printf "HTTP getLoginInfo response: %s\n" body_str
  >>= fun () ->
  let json = Yojson.Basic.from_string body_str in

  let open Yojson.Basic.Util in
  let data =
    json
    |> member "response"
    |> member "data"
  in
  let gcid_opt =
    match member "gcid" data with
    | `Null -> None
    | value -> (try Some (to_int value) with _ -> None)
  in

  match gcid_opt with
  | Some gcid ->
    Lwt.return (with_gcid config ~gcid)
  | None ->
    failwith "Failed to extract gscid from getLoginInfo response"

let jlogin_new config =
  let uri = Uri.of_string (api_url ^ "/jloginNew") in
  let headers =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
    |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
  in
  let gscid, password=
    match config.broker_config with
    | Greeksoft g -> g.gscid, g.password
  in
  let body_json =
    `Assoc [
      ("request", `Assoc [
        ("svcName", `String "jloginNew");
        ("svcGroup", `String "Login");
        ("data", `Assoc [
          ("pan_dob", `String "01/01/1901");
          ("version_no", `String "1.0.1.10");
          ("brokerid", `String "1");
          ("gscid", `String gscid);
          ("pass", `String (Digest.to_hex (Digest.string password))); (* md5 hash the password *)
        ])
      ])
    ]
  in
  let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string body_json) in

  Client.post ~headers ~body uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >>= fun body_str ->
  Lwt_io.printf "HTTP jloginNew response: %s\n" body_str
  >>= fun () ->
  let json = Yojson.Basic.from_string body_str in
  let session_id = 
    json 
    |> Yojson.Basic.Util.member "response" 
    |> Yojson.Basic.Util.member "sessionId"
    |> Yojson.Basic.to_string in
  Lwt_io.printf "session id is : %s\n" session_id 
  >>= fun () ->
  Lwt.return (with_session_id config ~session_id)


let place_order ~headers ~body =
  let uri = Uri.of_string (api_url ^ "/NewOrderRequest") in
  let wrapped_body =
    `Assoc [
      ("request", `Assoc [
        ("data", body);
        ("response_format", `String "json");
        ("request_type", `String "subscribe");
        ("streaming_type", `String "NewOrderRequest")
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

let cancel_order ~headers ~order_id =
  let uri = Uri.of_string (api_url ^ "/Order/" ^ order_id) in
  Client.delete ~headers uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >>= fun body_str ->
  Lwt_io.printf "Cancel Order HTTP Response: %s\n" body_str
  >>= fun () ->
  Lwt.return body_str


let get_strategy_positions ~headers ~gscid : Yojson.Basic.t list Lwt.t =
  let uri = Uri.of_string (api_url ^ "/getStrategyNameWiseNetPositionDetail?gscid=" ^ gscid) in
  Cohttp_lwt_unix.Client.get ~headers uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >>= fun body_str ->
  Lwt_io.printf "Monitor HTTP Response: %s\n" body_str
  >>= fun () ->
  let json = Yojson.Basic.from_string body_str in
  let data = Yojson.Basic.Util.member "data" json in
  match data with
  | `List lst -> Lwt.return lst
  | _ -> failwith "Expected a list of positions"


let get_order_status ~session_token ~gscid ~gorderid_opt =
  let gorderid = match gorderid_opt with
    | Some id -> id
    | None -> failwith "Missing broker_order_id"
  in
  let query = Uri.pct_encode ("greekOrderNo=" ^ gorderid ^ "&gscid=" ^ gscid) in
  let uri = Uri.of_string (api_url ^ "/getOrderDetail?" ^ query) in

  let headers =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Authorization" session_token
  in
  Cohttp_lwt_unix.Client.get ~headers uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >>= fun body_str ->
  Lwt_io.printf "Order status HTTP Response: %s\n" body_str
  >>= fun () ->
  let json = Yojson.Basic.from_string body_str in
  Lwt.return json
