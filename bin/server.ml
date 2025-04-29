open Opium
open Lwt.Infix

let respond_json (json : Yojson.Basic.t) =
  let body = Yojson.Basic.to_string json in
  Opium.Response.of_plain_text ~headers:(Headers.of_list [("Content-Type", "application/json")]) body
  |> Lwt.return

let login_handler req =
  Opium.Request.to_json req
  >>= fun body_opt ->
  let body = match body_opt with
    | Some json -> json
    | None -> failwith "Expected JSON body"
  in
  let open Yojson.Safe.Util in
  let username = body |> member "username" |> to_string in
  let password = body |> member "password" |> to_string in
  Om.Session.login ~username ~password
  >>= fun config ->
  Om.Session_store.set config;
  let json =
    `Assoc [
      ("session_token", `String config.session_token);
      ("user_id", `Int config.user_id)
    ]
  in
  respond_json json

let place_order_handler req =
  Lwt_io.printl "Received request for /order/place" >>= fun () ->
  Opium.Request.to_json req
  >>= fun body_opt ->
  let body = match body_opt with
    | Some json -> json
    | None -> failwith "Expected JSON body"
  in
  Lwt_io.printf "Request body: %s\n" (Yojson.Safe.to_string body) >>= fun () ->
  let order = Entities.Order.of_yojson body in
  Lwt_io.printf "Request body done: \n"  >>= fun () ->
  match Om.Session_store.get () with
  | Some config ->
    Lwt_io.printf "Request body done in Some: \n"  >>= fun () ->
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
    Lwt_io.printf "Request body done in None: \n"  >>= fun () ->
    failwith "No active session. Please log in first."

let () =
  App.empty
  |> App.post "/login" login_handler
  |> App.post "/order/place" place_order_handler
  |> App.run_command
