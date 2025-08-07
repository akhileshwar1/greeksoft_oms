(* src/rest_client.ml *)

open Lwt.Infix
open Cohttp_lwt_unix
open Entities.Config

let api_url = get_env_or_default "API_URL" "https://api.kite.trade/orders"
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


