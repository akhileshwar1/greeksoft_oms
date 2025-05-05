open Opium.Std
open Lwt.Infix
open Cohttp

(* Helper function to read the request body using App.json_of_body_exn and convert from Ezjsonm to Yojson *)
let json_of_request_body req =
  App.json_of_body_exn req (* Get the body as Ezjsonm.t Lwt.t using the Opium helper *)
  >>= fun (ezjsonm_json : Ezjsonm.t) -> (* Explicitly type the Ezjsonm result *)
  (* Convert Ezjsonm.t to Yojson.Basic.t via string representation *)
  let yojson_string = Ezjsonm.to_string ezjsonm_json in
  try
    Lwt.return (Yojson.Safe.from_string yojson_string)
  with
    | Yojson.Json_error err ->
    Lwt.fail (Failure (Printf.sprintf "Failed to parse JSON body after Ezjsonm conversion: %s" err))
    | exn ->
    Lwt.fail (Failure (Printf.sprintf "Unexpected error converting Ezjsonm to Yojson: %s" (Printexc.to_string exn)))

(* Note: respond_json can be simplified using respond' from Opium.Std *)
let respond_json (json : Yojson.Basic.t) =
  let body = Yojson.Basic.to_string json in
  respond' ~headers:(Header.of_list [("Content-Type", "application/json")]) (`String body)

let login_handler req =
  json_of_request_body req
  >>= fun json ->
  let open Yojson.Safe.Util in
  let username = json |> member "username" |> to_string in
  let password = json |> member "password" |> to_string in
  Om.Session.login ~username ~password
  >>= fun config ->
  Om.Session_store.set config;
  let response_json =
    `Assoc [
      ("session_token", `String config.session_token);
      ("user_id", `Int config.user_id)
    ]
  in
  respond_json response_json

let place_order_handler req =
  Lwt_io.printl "Received request for /order/place" >>= fun () ->
  json_of_request_body req
  >>= fun json ->
  Lwt_io.printf "Request body: %s\n" (Yojson.Safe.to_string json)
  >>= fun () ->
  let order = Entities.Order.of_yojson json in
  Lwt_io.printf "Request body done: \n"
  >>= fun () ->
  match Om.Session_store.get () with
  | Some config ->
    Lwt_io.printf "Request body done in Some: \n" >>= fun () ->
    Om.Order.place_order config order
    >>= fun updated_order ->
    let response_json =
      `Assoc [
        ("broker_order_id", `String (Option.value ~default:"" updated_order.broker_order_id));
        ("status", `String (updated_order.status
          |> Option.map Entities.Order.status_to_string
          |> Option.value ~default:"Unknown"))
      ]
    in
    respond_json response_json
  | None ->
    Lwt_io.printf "Request body done in None: \n" >>= fun () ->
    failwith "No active session. Please log in first."

let () =
  App.empty
  |> App.post "/login" login_handler
  |> App.post "/order/place" place_order_handler
  |> App.run_command
