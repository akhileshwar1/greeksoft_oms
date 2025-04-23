(* src/rest_client.ml *)

open Lwt.Infix
open Cohttp_lwt_unix
open Config

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
    Lwt.return (Config.with_session Config.empty ~session_token:session_token_opt ~user_id:user_id_opt)
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

  match iris_ip_opt, iris_port_opt with
  | Some iris_ip, Some iris_port ->
    Lwt.return (Config.with_iris config ~iris_ip ~iris_port)
  | _ ->
    failwith "Failed to extract iris_ip or iris_port from getFlagValues response"
