open Opium
open Lwt.Infix

let respond_json (json : Yojson.Basic.t) =
  let body = Yojson.Basic.to_string json in
  let headers = Opium.Headers.of_list [ ("Content-Type", "application/json") ] in
  Opium.Response.make ~headers ~body:(Body.of_string body) ()
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
  let json =
    `Assoc [
      ("session_token", `String config.session_token);
      ("user_id", `Int config.user_id)
    ]
  in
  respond_json json

let place_order_handler req =
  Opium.Request.to_json req
  >>= fun body_opt ->
  let body = match body_opt with
    | Some json -> json
    | None -> failwith "Expected JSON body"
  in
  let order = Entities.Order.of_yojson body in

  Om.Session.login ~username:"DHAN" ~password:"greek@123"
  >>= fun config ->
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

let () =
  App.empty
  |> App.post "/login" login_handler
  |> App.post "/order/place" place_order_handler
  |> App.run_command
