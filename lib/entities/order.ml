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
  tradingsymbol : string;
  exchange : string;
  quantity : int;
  lot : int;
  price : float;
  trigger_price : float;
  side : side;
  order_type : order_type;
  product : product_type;
  validity : validity_type;
  strategy_name : string option;
  broker_order_id : string option;
  status : status_type option;
  filled_quantity : int;
  filled_price : float;
  order_id : int;
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
    tradingsymbol = safe to_string "tradingsymbol";
    exchange = safe to_string "exchange";
    quantity = safe to_int "quantity";
    filled_quantity = safe to_int "quantity";
    filled_price = safe_to_float json "price";
    order_id = -1;
    lot = safe to_int "quantity" / 75;
    price = safe_to_float json "price";
    trigger_price = safe_to_float json "trigger_price";
    side = safe_match "side" to_string;
    order_type = safe_order_type "order_type" to_string;
    product = safe_product "product" to_string;
    validity = safe_validity "validity" to_string;
    strategy_name = (try json |> member "strategy_name" |> to_option to_string with _ -> None);
    broker_order_id = None;
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
    "tradingsymbol", `String order.tradingsymbol;
    "exchange", `String order.exchange;
    "quantity", `Int order.quantity;
    "filled_quantity", `Int order.filled_quantity;
    "lot", `Int order.lot;
    "price", `Float order.price;
    "filled_price", `Float order.filled_price;
    "trigger_price", `Float order.trigger_price;
    "side", `String (string_of_side order.side);
    "order_type", `String (string_of_order_type order.order_type);
    "product", `String (string_of_product order.product);
    "validity", `String (string_of_validity order.validity);
    "order_id", `Int order.order_id; 
  ] in

  let optional_fields =
    [ "strategy_name", Option.map (fun s -> `String s) order.strategy_name;
      "broker_order_id", Option.map (fun s -> `String s) order.broker_order_id;
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

  match safe to_string "order_status" with
  | "Executed" ->
    {
      tradingsymbol = safe to_string "symbol";
      exchange = "NSE";  (* Assuming fixed for now, or derive from instrument if needed *)
      quantity = int_of_string (safe to_string "qty");
      filled_quantity = int_of_string (safe to_string "traded_qty");
      filled_price = float_of_string (safe to_string "traded_price");
      price = float_of_string (safe to_string "price");
      lot = int_of_string (safe to_string "qty") / 75;
      trigger_price = 0.0;
      side = int_to_side (int_of_string (safe to_string "side"));
      order_type = int_to_order_type (int_of_string (safe to_string "order_type")) ;
      product = MIS;
      validity = DAY;
      strategy_name = safe_opt to_string "strategyName";
      broker_order_id = safe_opt to_string "gorderid";
      order_id = int_of_string (safe to_string "gorderid");
      status = Some Completed;
    }
  | "Partially Executed" ->
    {
      tradingsymbol = safe to_string "symbol";
      exchange = "NSE";  (* Assuming fixed for now, or derive from instrument if needed *)
      quantity = int_of_string (safe to_string "qty");
      filled_quantity = int_of_string (safe to_string "traded_qty");
      filled_price = float_of_string (safe to_string "traded_price");
      price = float_of_string (safe to_string "price");
      lot = int_of_string (safe to_string "qty") / 75;
      trigger_price = 0.0;
      side = int_to_side (int_of_string (safe to_string "side"));
      order_type = int_to_order_type (int_of_string (safe to_string "order_type")) ;
      product = MIS;
      validity = DAY;
      strategy_name = safe_opt to_string "strategyName";
      broker_order_id = safe_opt to_string "gorderid";
      order_id = int_of_string (safe to_string "gorderid");
      status = Some Pending;
    }
  | "Pending" ->
    {
      tradingsymbol = safe to_string "symbol";
      exchange = "NSE";  (* Assuming fixed for now, or derive from instrument if needed *)
      quantity = int_of_string (safe to_string "qty");
      filled_quantity = int_of_string (safe to_string "qty") - int_of_string (safe to_string "pending_qty");
      filled_price = 0.0; (*since there is no fill price in pending order type*)
      price = float_of_string (safe to_string "price");
      lot = int_of_string (safe to_string "qty") / 75;
      trigger_price = 0.0;
      side = int_to_side (int_of_string (safe to_string "side"));
      order_type = Limit;
      product = MIS;
      validity = DAY;
      strategy_name = safe_opt to_string "strategyName";
      broker_order_id = safe_opt to_string "gorderid";
      order_id = int_of_string (safe to_string "gorderid");
      status = Some Pending;
    }
  | _ -> 
    {
      tradingsymbol = safe to_string "symbol";
      exchange = "NSE";  (* Assuming fixed for now, or derive from instrument if needed *)
      quantity = -1;
      lot = -1;
      price = 0.0;       (* Same as above — parse from `reason` if required *)
      trigger_price = 0.0;
      side = int_to_side (int_of_string (safe to_string "side"));
      order_type = Limit;
      product = MIS;
      validity = DAY;
      strategy_name = safe_opt to_string "strategyName";
      broker_order_id = safe_opt to_string "gorderid";
      status = Some Rejected;
      filled_quantity = -1;
      filled_price = 0.0;
      order_id = int_of_string (safe to_string "gorderid");
    }
