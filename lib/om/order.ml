(* om order management *)
open Entities.Order
open Lwt.Infix
open Yojson.Basic.Util

(* Map to Greeksoft API codes *)
let side_to_int = function Buy -> 1 | Sell -> 2
let order_type_to_int = function Limit -> 1 | Market -> 2
let product_type_to_int = function CNC -> 0 | NRML -> 1 | MIS -> 2
let validity_type_to_int = function DAY -> 0 | IOC -> 1

let normalize_data_symbol (data_symbol: string) : string =
  (* Strip exchange and year *)
  match String.split_on_char ':' data_symbol with
  | [_exchange; raw] ->
    raw 
  | _ -> data_symbol  

let adjust_order_quantity quantity =
  let lot_size = 75 in
  let lot = quantity / lot_size in
  let adjusted_quantity = lot * lot_size in
  (lot, adjusted_quantity)

(* Create JSON for Greeksoft from Order.t and Config.t *)
let to_json (config : Entities.Config.t) (order : Entities.Order.t) : Yojson.Basic.t =
  match config.broker_config with
  | Greeksoft g ->
      let tradingsymbol = normalize_data_symbol order.tradingsymbol in

    let () = Printf.printf "tradingsymbol is %s\n%!" tradingsymbol in
    let gtoken = Greeksoft.Contracts_store.get_token ~symbol:tradingsymbol in
    let gtoken_str =
      match gtoken with
      | Some token -> string_of_int token
      | None -> ""  (* or some default string or fail with an error *)
      in
    let tradingsymbol = Greeksoft.Contracts_store.get_symbol ~token:(Option.get gtoken) in
    let (lot, quantity) = adjust_order_quantity order.quantity in
    
    let () = Printf.printf "gtoken is %s, tradesymbol is %s \n%!" gtoken_str (Option.get tradingsymbol) in

    let corderid = "3" in
      `Assoc [
        ("trigger_price", `String (string_of_float order.trigger_price));
        ("gtoken", `String gtoken_str);
        ("side", `String (string_of_int (side_to_int order.side)));
        ("gcid", `Int g.gcid);
        ("validity", `String (string_of_int (validity_type_to_int order.validity)));
        ("price", `String (string_of_float order.price));
        ("exchange", `String order.exchange);
        ("disclosed_qty", `String "0");
        ("tradeSymbol", `String "NIFTY");
        ("lot", `String (string_of_int lot));
        ("order_type", `String (string_of_int (order_type_to_int order.order_type)));
        ("product", `String (string_of_int (product_type_to_int order.product))); (*Cnc for delivery*)
        ("qty", `String (string_of_int quantity));
        ("corderid", `String corderid);
        ("amo", `String "0");
        ("iprocli", `String "2");
        ("gtdExpiry", `Int 0);
        ("is_post_closed", `String "0");
        ("is_preopen_order", `String "0");
        ("isSqOffOrder", `String "false");
        ("offline", `String "0");
        ("is_restapi", `String "1");
        ("strategyName", `String (match order.strategy_name with Some s -> s | None -> ""))
      ]

(* OMS-wide place_order interface *)
let place_order (config : Entities.Config.t) (order : Entities.Order.t) =
  let headers =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
    |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
  in
  match config.broker_config with
  | Greeksoft _ ->
    Printf.printf "in match greeksoft%!";
    let json = to_json config order in
    Greeksoft.Rest_client.place_order ~headers ~body:json
    >>= fun body_str ->
    let json = Yojson.Basic.from_string body_str in
    let open Yojson.Basic.Util in
    let gorderid_opt =
      json
      |> member "response"
      |> member "data"
      |> member "gorderid"
      |> to_string_option
    in
    begin match gorderid_opt with
      | Some gorderid ->
        let updated_order = { order with broker_order_id = Some gorderid } in
        Lwt.return updated_order
      | None ->
        failwith "gorderid missing in order response"
      end

let cancel_order (config : Entities.Config.t) (order : Entities.Order.t) =
  let headers =
  Cohttp.Header.init ()
  |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
  |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
  in
  match config.broker with
  | "greeksoft" ->
    Greeksoft.Rest_client.cancel_order ~headers ~order_id:(Option.get order.broker_order_id)
    >>= fun body_str ->
    let json = Yojson.Basic.from_string body_str in
    let open Yojson.Basic.Util in
    let success =
      json
      |> member "success"
    in
    begin match Yojson.Basic.Util.to_string success with
      | "true" ->
        let updated_order = { order with status = Some Cancelled} in
        Lwt.return updated_order
      | _ ->
        failwith "Not cancelled!"
      end
  | _ ->
    failwith "Unsupported broker"

let get_order_status (config : Entities.Config.t) (order : Entities.Order.t) : Entities.Order.t Lwt.t =
  match config.broker with
  | "greeksoft" ->
    let gorderid_opt = order.broker_order_id in
    let gscid =
      match config.broker_config with
      | Greeksoft g -> g.gscid
    in
    let token = config.session_token in

    Greeksoft.Rest_client.get_order_status
      ~session_token:token
      ~gscid
      ~gorderid_opt
    >>= fun json ->
    let data = member "data" json in
    begin match data with
      | `List (first :: _) ->
        let status_str = member "order_status" first |> to_string in
        let status =
          status_str
          |> Greeksoft.Order.greeksoft_string_to_status
          |> Greeksoft.Order.om_status
        in
        let traded_qty = member "traded_qty" first |> to_int in
        let updated_order = {
          order with
          status = Some status;
          quantity = traded_qty;
        } in
        Lwt.return updated_order
      | _ ->
        failwith "Could not extract order status from response"
      end
  | _ ->
    failwith "Unsupported broker"
