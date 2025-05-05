(* Our order entity *)

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
  price : float;
  trigger_price : float;
  side : side;
  order_type : order_type;
  product : product_type;
  validity : validity_type;
  strategy_name : string option;
  broker_order_id : string option;
  status : status_type option;
}



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
    price = safe to_float "price";
    trigger_price = safe to_float "trigger_price";
    side = safe_match "side" to_string;
    order_type = safe_order_type "order_type" to_string;
    product = safe_product "product" to_string;
    validity = safe_validity "validity" to_string;
    strategy_name = (try json |> member "strategy_name" |> to_option to_string with _ -> None);
    broker_order_id = None;
    status = None;
  }

