(* src/rest_client.ml *)

open Lwt.Infix
open Cohttp_lwt_unix

let base_url = "http://greekapi.greeksoft.in:3001"

let login ~username ~password =
  let uri = Uri.of_string (base_url ^ "/auth/greek/sessiontoken") in
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

  let find_string_opt key json =
    match Yojson.Basic.Util.member key json with
    | `Null -> None
    | value ->
      try Some (Yojson.Basic.Util.to_string value)
      with _ -> None
  in

  let find_int_opt key json =
    match Yojson.Basic.Util.member key json with
    | `Null -> None
    | value ->
      try Some (Yojson.Basic.Util.to_int value)
      with _ -> None
  in

  let session_token_opt = find_string_opt "sessionToken" json in
  let user_id_opt = find_int_opt "id" json in
  match session_token_opt, user_id_opt with
  | Some session_token_opt, Some user_id_opt ->
    Lwt.return (Config.with_session Config.empty ~session_token:session_token_opt ~user_id:user_id_opt)
  | _ ->
    failwith "Failed to extract session_token or user_id from login response"
