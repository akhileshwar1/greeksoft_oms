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
  let open Yojson.Safe.Util in
  {
    tradingsymbol = json |> member "tradingsymbol" |> to_string;
    exchange = json |> member "exchange" |> to_string;
    quantity = json |> member "quantity" |> to_int;
    price = json |> member "price" |> to_float;
    trigger_price = json |> member "trigger_price" |> to_float;
    side = (match json |> member "side" |> to_string with
      | "Buy" -> Buy | "Sell" -> Sell | _ -> Buy);
    order_type = (match json |> member "order_type" |> to_string with
      | "Limit" -> Limit | "Market" -> Market | _ -> Limit);
    product = (match json |> member "product" |> to_string with
      | "MIS" -> MIS | "CNC" -> CNC | "NRML" -> NRML | _ -> MIS);
    validity = (match json |> member "validity" |> to_string with
      | "DAY" -> DAY | "IOC" -> IOC | _ -> DAY);
    strategy_name = json |> member "strategy_name" |> to_option to_string;
    broker_order_id = None; (* set later by OMS *)
    status = None;
  }
