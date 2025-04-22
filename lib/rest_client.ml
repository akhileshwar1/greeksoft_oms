(* src/rest_client.ml *)

open Lwt.Infix
open Cohttp_lwt_unix

let base_url = "http://182.76.70.89:3001"

let login ~username ~password =
  let uri = Uri.of_string (base_url ^ "/auth/greek/sessiontoken") in
  let headers = Cohttp.Header.init_with "Content-Type" "application/json" in
  let body_json = `Assoc [
    ("user_id", `String username);
    ("password", `String password)
  ] in
  let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string body_json) in

  Client.post ~headers ~body uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >|= fun body_str ->
  let json = Yojson.Basic.from_string body_str in
  let open Yojson.Basic.Util in
  let session_token = json |> member "session_token" |> to_string in
  let user_id = json |> member "user_id" |> to_string in
  Config.with_session Config.empty ~session_token ~user_id
