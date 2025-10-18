(* Our order entity *)
(* NOTE: the transformation functions in here are greeksoft specific, needs to be generalized for all brokers *)
type side =
  | Buy
  | Sell

type order_type =
  | Limit
  | Market

type product_type =
  | CNC
  | NRML
  | MIS

type validity_type =
  | DAY
  | IOC

type status_type =
  | Cancelled
  | Pending
  | Rejected
  | Completed
  | Unknown

let string_to_status = function
  | "Cancelled" -> Cancelled
  | "Pending" -> Pending
  | "Rejected" -> Rejected
  | "Completed" -> Completed
  | _ -> Unknown (* Fallback *)

let status_to_string = function
  | Cancelled -> "Cancelled"
  | Pending -> "Pending"
  | Rejected -> "Rejected"
  | Completed -> "Completed"
  | _ -> "Unknown"

type t = {
  placed_at : Ptime.t option;
  executed_at : Ptime.t option;
  tradingsymbol : string;
  exchange : string;
  quantity : float;
  lot : int;
  price : float;
  trigger_price : float;
  side : side;
  order_type : order_type;
  product : product_type;
  validity : validity_type;
  strategy_name : string option;
  broker_order_id : string;
  status : status_type option;
  filled_quantity : float;
  filled_price : float;
  order_id : string;
}

(* to fix the errors resulting from decimal json values like 100.0 being taken as int instead of float *)
let safe_to_float json key =
  let open Yojson.Safe.Util in
  match member key json with
  | `Float f -> f
  | `Int i -> float_of_int i
  | `String s -> float_of_string s
  | v ->
    Printf.printf "Unexpected type for key '%s': %s\n%!" key (to_string v);
    failwith ("Expected float/int/string for key: " ^ key)

let ptime_of_string (s : string) : Ptime.t option =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> Some t
  | Error _ -> None 

(* for incoming order requests from strategy *)
let of_yojson (json : Yojson.Safe.t) : t =
  Printf.printf "in yojson\n%!";

  let open Yojson.Safe.Util in
  let safe f key =
    try f (json |> member key)
    with e ->
      Printf.printf "Error extracting key '%s': %s\n%!" key (Printexc.to_string e);
      raise e
  in

  let safe_match key f = 
    try match f (json |> member key) with
      | "Buy" -> Buy
      | "Sell" -> Sell
      | other -> Printf.printf "Unknown side value '%s', defaulting to Buy\n%!" other; Buy
    with e ->
      Printf.printf "Error parsing '%s': %s\n%!" key (Printexc.to_string e);
      Buy
  in

  let safe_order_type key f = 
    try match f (json |> member key) with
      | "Limit" -> Limit
      | "Market" -> Market
      | other -> Printf.printf "Unknown order_type '%s', defaulting to Limit\n%!" other; Limit
    with e ->
      Printf.printf "Error parsing '%s': %s\n%!" key (Printexc.to_string e);
      Limit
  in

  let safe_product key f = 
    try match f (json |> member key) with
      | "MIS" -> MIS
      | "CNC" -> CNC
      | "NRML" -> NRML
      | other -> Printf.printf "Unknown product '%s', defaulting to MIS\n%!" other; MIS
    with e ->
      Printf.printf "Error parsing '%s': %s\n%!" key (Printexc.to_string e);
      MIS
  in

  let safe_validity key f = 
    try match f (json |> member key) with
      | "DAY" -> DAY
      | "IOC" -> IOC
      | other -> Printf.printf "Unknown validity '%s', defaulting to DAY\n%!" other; DAY
    with e ->
      Printf.printf "Error parsing '%s': %s\n%!" key (Printexc.to_string e);
      DAY
  in

  {
    placed_at = ptime_of_string (safe to_string "placed_at");
    executed_at = None;
    tradingsymbol = safe to_string "tradingsymbol";
    exchange = safe to_string "exchange";
    quantity = safe_to_float json "quantity";
    filled_quantity = safe_to_float json "filled_quantity";
    filled_price = safe_to_float json "price";
    order_id = safe to_string "order_id";
    lot = (int_of_float ((safe_to_float json "quantity") /. 75.0));
    price = safe_to_float json "price";
    trigger_price = safe_to_float json "trigger_price";
    side = safe_match "side" to_string;
    order_type = safe_order_type "order_type" to_string;
    product = safe_product "product" to_string;
    validity = safe_validity "validity" to_string;
    strategy_name = (try json |> member "strategy_name" |> to_option to_string with _ -> None);
    broker_order_id = safe to_string "broker_order_id";
    status = Some Pending;
  }

(* currently in use for sending order update to strategy *)
let to_yojson (order : t) : Yojson.Safe.t =
  let string_of_side = function
    | Buy -> "Buy"
    | Sell -> "Sell"
  in

  let string_of_order_type = function
    | Limit -> "Limit"
    | Market -> "Market"
  in

  let string_of_product = function
    | MIS -> "MIS"
    | CNC -> "CNC"
    | NRML -> "NRML"
  in

  let string_of_validity = function
    | DAY -> "DAY"
    | IOC -> "IOC"
  in

  let base_fields = [
    ("placed_at", 
      match order.placed_at with
      | Some ts -> `String (Ptime.to_rfc3339 ts)
      | None -> `Null);
    ("executed_at", 
      match order.executed_at with
      | Some ts -> `String (Ptime.to_rfc3339 ts)
      | None -> `Null);
    "tradingsymbol", `String order.tradingsymbol;
    "exchange", `String order.exchange;
    "quantity", `Float order.quantity;
    "filled_quantity", `Float order.filled_quantity;
    "lot", `Int order.lot;
    "price", `Float order.price;
    "filled_price", `Float order.filled_price;
    "trigger_price", `Float order.trigger_price;
    "side", `String (string_of_side order.side);
    "order_type", `String (string_of_order_type order.order_type);
    "product", `String (string_of_product order.product);
    "validity", `String (string_of_validity order.validity);
    "order_id", `String order.order_id; 
    "broker_order_id", `String order.broker_order_id;
  ] in

  let optional_fields =
    [ "strategy_name", Option.map (fun s -> `String s) order.strategy_name;
      "status", Option.map (fun s -> `String (status_to_string s)) order.status ]
    |> List.filter_map (fun (k, v_opt) -> Option.map (fun v -> k, v) v_opt)
  in

  `Assoc (base_fields @ optional_fields)

let int_to_side side = 
  match side with
  | 1 -> Buy 
  | 2 -> Sell
  | _ -> Sell

let int_to_order_type order_type =
  match order_type with
  | 1 -> Limit
  | 2 -> Market
  | _ -> Limit

(* to parse order update from broker *)
let ws_of_yojson (data: Yojson.Safe.t) : t =
  Printf.printf "in ws_of_yojson\n%!";

  let open Yojson.Safe.Util in

  let safe f key =
    try f (data |> member key)
    with e ->
      Printf.printf "Error extracting key '%s': %s\n%!" key (Printexc.to_string e);
      raise e
  in

  let safe_opt f key =
    try Some (f (data |> member key))
    with _ -> None
  in

  let common_fields  =
    {
      placed_at = None;
      status = Some Completed; (* placeholder to avoid compile time error *)
      executed_at = Some (Ptime_clock.now ());
      tradingsymbol = safe to_string "symbol";
      exchange = "NSE";  (* Assuming fixed for now *)
      quantity = float_of_string (safe to_string "qty");
      filled_quantity = float_of_string (safe to_string "traded_qty");
      filled_price = float_of_string (safe to_string "traded_price");
      price = float_of_string (safe to_string "price");
      lot = int_of_string (safe to_string "qty") / 75;
      trigger_price = 0.0;
      side = int_to_side (int_of_string (safe to_string "side"));
      order_type = int_to_order_type (int_of_string (safe to_string "order_type"));
      product = MIS;
      validity = DAY;
      strategy_name = safe_opt to_string "strategyName";
      broker_order_id = safe to_string "gorderid";
      order_id = ""; (* since the websocket update won't have our order id *)
    }
  in

  match safe to_string "order_status" with
  | "Executed" ->
    { common_fields  with status = Some Completed }
  | "Partially Executed" ->
    { common_fields  with status = Some Pending }
  | "Pending" ->
    {
      common_fields with
      filled_quantity = float_of_string (safe to_string "qty") -. float_of_string (safe to_string "pending_qty");
      filled_price = 0.0; (* no fill price in pending orders *)
      status = Some Pending;
    }
  | _ -> 
    {
      common_fields with
      quantity = -1.0;
      lot = -1;
      price = 0.0;
      side = Sell; (* side not available in Rms Rejected status *)
      order_type = Limit;
      status = Some Rejected;
      filled_quantity = -1.0;
      filled_price = 0.0;
    }

(* to parse order update from zerodha broker *)
let zerodha_ws_of_yojson (data: Yojson.Safe.t) : t =
  Printf.printf "in ws_of_yojson\n%!";

  let open Yojson.Safe.Util in

  let safe f key =
    try f (data |> member key)
    with e ->
      Printf.printf "Error extracting key '%s': %s\n%!" key (Printexc.to_string e);
      raise e
  in

  let safe_opt f key =
    try Some (f (data |> member key))
    with _ -> None
  in

  let common_fields  =
    {
      placed_at = None;
      status = Some Completed; (* placeholder to avoid compile time error *)
      executed_at = Some (Ptime_clock.now ());
      tradingsymbol = safe to_string "symbol";
      exchange = "NSE";  (* Assuming fixed for now *)
      quantity = float_of_string (safe to_string "quantity");
      filled_quantity = float_of_string (safe to_string "filled_quantity");
      filled_price = float_of_string (safe to_string "average_price");
      price = float_of_string (safe to_string "price");
      lot = int_of_string (safe to_string "qty") / 75;
      trigger_price = 0.0;
      side = int_to_side (int_of_string (safe to_string "side"));
      order_type = int_to_order_type (int_of_string (safe to_string "order_type"));
      product = MIS;
      validity = DAY;
      strategy_name = safe_opt to_string "strategyName";
      broker_order_id = safe to_string "gorderid";
      order_id = ""; (* since the websocket update won't have our order id *)
    }
  in

  match safe to_string "order_status" with
  | "COMPLETE" ->
    { common_fields  with status = Some Completed }
  | "UPDATE" ->
    { common_fields  with status = Some Pending }
  | _ -> 
    {
      common_fields with
      quantity = -1.0;
      lot = -1;
      price = 0.0;
      side = Sell; (* side not available in Rms Rejected status *)
      order_type = Limit;
      status = Some Rejected;
      filled_quantity = -1.0;
      filled_price = 0.0;
    }


let binance_ws_of_yojson (data : Yojson.Safe.t) : t =
  let open Yojson.Safe.Util in

  (* helpers to safely extract strings/numbers *)
  let safe_string key =
    try (data |> member key |> to_string) with _ -> ""
  in
  let safe_float_of_string s =
    try float_of_string s with _ -> 0.0
  in

  (* let safe_int_of_string s = *)
  (*   try int_of_float (float_of_string s) with _ -> 0 *)
  (* in *)

  (* Binance executionReport fields (common): *)
  let symbol = (try data |> member "s" |> to_string with _ -> safe_string "symbol") in
  let side_s = try data |> member "S" |> to_string with _ -> safe_string "side" in
  let side =
    match String.uppercase_ascii side_s with
    | "BUY" -> Buy
    | "SELL" -> Sell
    | _ -> Buy
  in
  let qty_s = (try data |> member "q" |> to_string with _ -> safe_string "q") in
  let executed_qty_s = (try data |> member "z" |> to_string with _ -> safe_string "z") in
  let price_s = (try data |> member "p" |> to_string with _ -> safe_string "p") in
  let last_filled_price_s = (try data |> member "L" |> to_string with _ -> "") in
  let status_s =
    try data |> member "X" |> to_string
    with _ -> (try data |> member "x" |> to_string with _ -> safe_string "X")
  in

  (* map Binance status -> our status_type *)
  let status =
    match String.uppercase_ascii status_s with
    | "FILLED" -> Some Completed
    | "PARTIALLY_FILLED" -> Some Pending
    | "CANCELED" | "CANCELLED" -> Some Cancelled
    | "REJECTED" -> Some Rejected
    | "NEW" -> Some Pending
    | _ -> Some Unknown
  in

  let qty = if qty_s = "" then 0.0 else safe_float_of_string qty_s in
  let filled_qty = if executed_qty_s = "" then 0.0 else safe_float_of_string executed_qty_s in
  let price =
    if last_filled_price_s <> "" then safe_float_of_string last_filled_price_s
    else if price_s <> "" then safe_float_of_string price_s
    else 0.0
  in

  let broker_order_id =
    try data |> member "i" |> to_int |> string_of_int (* orderId *)
    with _ -> safe_string "orderId"
  in

  (* create Entities.Order.t value *)
  {
    placed_at = None;
    executed_at = Some (Ptime_clock.now ());
    tradingsymbol = symbol;
    exchange = "BINANCE";
    quantity = qty;
    lot = (if qty = 0.0 then 0 else int_of_float qty); (* adapt lot computation later *)
    price = price;
    trigger_price = 0.0;
    side = side;
    order_type = Limit; (* Binance reports "o" for order type if you want to parse it *)
    product = MIS;
    validity = DAY;
    strategy_name = None;
    broker_order_id = broker_order_id;
    status = status;
    filled_quantity = filled_qty;
    filled_price = price;
    order_id = ""; (* not provided by strategy; use your own mapping if needed *)
  }
