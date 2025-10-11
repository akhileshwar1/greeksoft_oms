(* om order management *)
(* Acts as a transformer between strategy layer and the brokers *)
open Entities.Order
open Lwt.Infix
open Yojson.Basic.Util

let uuid = Uuidm.v4_gen (Random.State.make_self_init ())
let generate_order_id () : string =
  Uuidm.to_string (uuid ())

(* Map to Greeksoft API codes *)
let side_to_int = function Buy -> 1 | Sell -> 2
let order_type_to_int = function Limit -> 1 | Market -> 2
let product_type_to_int = function CNC -> 0 | NRML -> 1 | MIS -> 2
let validity_type_to_int = function DAY -> 0 | IOC -> 1

(* Map to Zerodha API specs *)
let side_to_z_string = function Buy -> "BUY" | Sell -> "SELL"
let otype_to_z_string = function Limit -> "LIMIT" | Market -> "MARKET"
let vtype_to_z_string = function DAY -> "DAY" | IOC -> "IOC"

let normalize_data_symbol (data_symbol: string) : string =
  (* Strip exchange and year *)
  match String.split_on_char ':' data_symbol with
  | [_exchange; raw] ->
    raw 
  | _ -> data_symbol  

(* optional: used for signature *)
(* Using Digestif + Cstruct for HMAC-SHA256. Add (digestif cstruct) to your dune deps. *)
module SHA = Digestif.SHA256

let timestamp_ms () = 
  Int64.to_string (Int64.of_float (Unix.gettimeofday () *. 1000.0))

(* Add standard Binance params: symbol, side, type, price/quantity etc.
   Return (params_list, pretty_json_for_logging) *)
let build_binance_params_of_order (config : Entities.Config.t) (order : Entities.Order.t) =
  match config.broker_config with
  | Binance _ ->
    let symbol = order.tradingsymbol  (* ensure correct symbol format e.g. BTCUSDT *) in
    let side = match order.side with Buy -> "BUY" | Sell -> "SELL" in
    let otype = match order.order_type with Limit -> "LIMIT" | Market -> "MARKET" in
    let params =
      [
        ("symbol", `String symbol);
        ("side", `String side);
        ("type", `String otype);
        ("timestamp", `String (timestamp_ms ()));
      ]
    in
    let params =
      (* include price/quantity where applicable *)
      let params = 
        if otype = "LIMIT" then
          ("price", `String (Printf.sprintf "%.8f" order.price)) :: ("timeInForce",`String "GTC") :: params
        else params
      in
      (* quantity as string *)
      ("quantity", `String (string_of_int order.quantity)) :: params
    in
    (* optional client id *)
    let params = ("newClientOrderId", `String (match order.order_id with "" -> generate_order_id () | s -> s)) :: params in
    let json_log = `Assoc [
      ("symbol", `String symbol);
      ("side", `String side);
      ("type", `String otype);
      ("price", `Float order.price);
      ("quantity", `Int order.quantity)
    ] in
    (`Assoc params, json_log)
  | _ -> failwith "build_binance_params_of_order: not a Binance config"

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
    
    let () = Printf.printf "gtoken is %s, tradesymbol is %s \n%!" gtoken_str (Option.get tradingsymbol) in

    let corderid = "3" in
      `Assoc [
        ("trigger_price", `String (string_of_float order.trigger_price));
        ("gtoken", `String gtoken_str);
        ("side", `String (string_of_int (side_to_int order.side)));
        ("gcid", `String (string_of_int g.gcid));
        ("validity", `String (string_of_int (validity_type_to_int order.validity)));
        ("price", `String (string_of_float order.price));
        ("exchange", `String order.exchange);
        ("disclosed_qty", `String "0");
        ("tradeSymbol", `String "NIFTY");
        ("lot", `String (string_of_int order.lot));
        ("order_type", `String (string_of_int (order_type_to_int order.order_type)));
        ("product", `String (string_of_int (product_type_to_int order.product))); (*Cnc for delivery*)
        ("qty", `String (string_of_int order.quantity));
        ("corderid", `String corderid);
        ("amo", `String "0");
        (* ("iprocli", `String "2"); *)
        ("iprocli", `String (Entities.Config.get_env_or_default "IPROCLI" "2")); (* for retailer id in live it is 0*)
      ("settlor", `String (Entities.Config.get_env_or_default "GREEKSOFT_SETTLOR" "ORBIS0009680"));
        ("gtdExpiry", `Int 0);
        ("is_post_closed", `String "0");
        ("is_preopen_order", `String "0");
        ("isSqOffOrder", `String "false");
        ("offline", `String "0");
        ("is_restapi", `String "1");
        ("algoId", `String ""); (*135803*)
        ("AccountNumber", `String "");
        ("strategyName", `String (match order.strategy_name with Some s -> s | None -> ""))
      ]
  | Zerodha _ ->
    `Assoc [
      ("tradingsymbol", `String order.tradingsymbol);
      ("exchange", `String "NSE");
      ("transaction_type", `String (side_to_z_string order.side));
      ("order_type", `String (otype_to_z_string order.order_type));
      ("quantity", `String (string_of_int order.quantity));
      ("validity", `String (vtype_to_z_string order.validity));
    ]
  | Binance _ ->
      (* Return a JSON form useful for logging/debugging. The actual HTTP will be form-encoded signed query. *)
      let (params, _json_log) = build_binance_params_of_order config order in
      params
  | _ -> `Assoc [] 


let place rest_call headers json order_tag order = 
  Printf.printf "in match greeksoft%!";
  Printf.printf " Order is: %s\n%!" (Yojson.Basic.pretty_to_string json);
  rest_call ~headers ~body:json
  >>= fun body_str ->
  let json = Yojson.Basic.from_string body_str in
  let open Yojson.Basic.Util in
  let gorderid_opt =
    json
    |> member "response"
    |> member "data"
    |> member order_tag 
    |> to_string_option
  in
  begin match gorderid_opt with
    | Some gorderid ->
      let updated_order = { order with broker_order_id = gorderid } in
      Lwt.return updated_order
    | None ->
      failwith "gorderid missing in order response"
    end

(* OMS-wide place_order interface *)
let place_order (config : Entities.Config.t) (order : Entities.Order.t) =
  let headers =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
    |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
  in
  let json = to_json config order in
  match config.broker_config with
  | Greeksoft _ ->
    place Greeksoft.Rest_client.place_order headers json "gorderid" order 
  | Zerodha _ ->
    place Zerodha.Rest_client.place_order headers json "order_id" order 
  | Binance bcfg ->
      Printf.printf " in binance place order \n%!";
      let api_key = bcfg.api_key in
      let secret_key = bcfg.secret_key in
    (* For Binance call binance rest_client directly. We expect Binanace.Rest_client.place_order
       to implement signing and return the raw response string. *)
    Binance.Rest_client.place_order ~headers ~body:json ~api_key ~secret_key
    >>= fun body_str ->

      Printf.printf " in binance place order returned \n%!";
    (* parse Binance response (flat JSON) and extract orderId or clientOrderId *)
    let j = Yojson.Basic.from_string body_str in
    let open Yojson.Basic.Util in
    let order_id_opt =
      (try Some (j |> member "orderId" |> to_string) with _ -> None)
      |> (function None -> (try Some (j |> member "clientOrderId" |> to_string) with _ -> None) | s -> s)
    in
    begin match order_id_opt with
      | Some oid ->
          let updated_order = { order with broker_order_id = oid } in
          Lwt.return updated_order
      | None ->
          Lwt.fail_with ("Binance.place_order: no orderId in response: " ^ body_str)
    end
  | Dummy _ ->
    Lwt.return { order with broker_order_id = generate_order_id ()}
   

let cancel_order (config : Entities.Config.t) (order : Entities.Order.t) =
  let headers =
  Cohttp.Header.init ()
  |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
  |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
  in
  match config.broker_config with
  | Greeksoft _ ->
    Greeksoft.Rest_client.cancel_order ~headers ~order_id: order.broker_order_id
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
  | Binance bcfg ->
      (* For Binance we need symbol and either orderId or origClientOrderId.
         We call the Binance client: cancel_order ~headers ~symbol ~order_id ~api_key ~secret_key *)
      let api_key = bcfg.api_key in
      let secret_key = bcfg.secret_key in
      let symbol = order.tradingsymbol in
      (* prefer broker_order_id (exchange orderId), else fallback to order.order_id (client id) *)
      let maybe_order_id = 
        if order.broker_order_id <> "" then Some order.broker_order_id
        else if order.order_id <> "" then Some order.order_id
        else None
      in
      begin match maybe_order_id with
      | None -> Lwt.fail_with "cancel_order: no broker_order_id or client order_id available for Binance"
      | Some oid ->
          Binance.Rest_client.cancel_order
            ~headers
            ~symbol
            ~order_id:oid
            ~api_key
            ~secret_key
          >>= fun body_str ->
          (* parse response and map to Entities.Order.t *)
          let j =
            try Yojson.Basic.from_string body_str
            with _ -> `Assoc []
          in
          let open Yojson.Basic.Util in
          (* Binance cancel response typically contains "status": "CANCELED" and fields like orderId, origClientOrderId *)
          let status_opt =
            try Some (j |> member "status" |> to_string) with _ -> None
          in
          begin match status_opt with
          | Some s when String.uppercase_ascii s = "CANCELED" || String.uppercase_ascii s = "CANCELLED" ->
              Lwt.return { order with status = Some Cancelled }
          | Some _ ->
              (* some other state returned — keep Unknown or set accordingly *)
              Lwt.return { order with status = Some Unknown }
          | None ->
              (* No status field: if HTTP was 200 we treat as cancelled; else caller will inspect body_str *)
              Lwt.return { order with status = Some Cancelled }
          end
      end
  | _ ->
    failwith "Unsupported broker"

let get_order_status (config : Entities.Config.t) (order : Entities.Order.t) : Entities.Order.t Lwt.t =
  match config.broker with
  | "greeksoft" ->
    let gorderid_opt = Some order.broker_order_id in
    let gscid =
      match config.broker_config with
      | Greeksoft g -> g.gscid
      | _ -> failwith "unsupported broker"
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
