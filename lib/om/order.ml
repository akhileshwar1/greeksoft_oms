(* om order management *)
open Entities.Order
open Lwt.Infix

(* Map to Greeksoft API codes *)
let side_to_int = function Buy -> 1 | Sell -> 2
let order_type_to_int = function Limit -> 1 | Market -> 2
let product_type_to_int = function CNC -> 0 | NRML -> 1 | MIS -> 2
let validity_type_to_int = function DAY -> 0 | IOC -> 1

(* Create JSON for Greeksoft from Order.t and Config.t *)
let to_greeksoft_json (config : Entities.Config.t) (order : Entities.Order.t) : Yojson.Basic.t =
  match config.broker_config with
  | Greeksoft g ->
      let gtoken = "101001232" in
      let corderid = "3" in
      `Assoc [
        ("trigger_price", `String (string_of_float order.trigger_price));
        ("gtoken", `String gtoken);
        ("side", `String (string_of_int (side_to_int order.side)));
        ("gcid", `Int g.gcid);
        ("validity", `String (string_of_int (validity_type_to_int order.validity)));
        ("price", `String (string_of_float order.price));
        ("exchange", `String order.exchange);
        ("disclosed_qty", `String "0");
        ("tradeSymbol", `String order.tradingsymbol);
        ("lot", `String "1");
        ("order_type", `String (string_of_int (order_type_to_int order.order_type)));
        ("product", `String (string_of_int (product_type_to_int order.product)));
        ("qty", `String (string_of_int order.quantity));
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
  let json = to_greeksoft_json config order in
  let headers =
  Cohttp.Header.init ()
  |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
  |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
  in
  match config.broker with
  | "greeksoft" ->
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
  | _ ->
    failwith "Unsupported broker"
