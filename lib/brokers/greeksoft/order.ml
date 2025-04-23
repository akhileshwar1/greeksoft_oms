(* greeksoft order *)
open Entities.Order
(* Map OCaml types to Greeksoft API codes *)
let side_to_int = function
  | Buy -> 1
  | Sell -> 2

let order_type_to_int = function
  | Limit -> 1
  | Market -> 2

let product_type_to_int = function
  | CNC -> 0
  | NRML -> 1
  | MIS -> 2

let validity_type_to_int = function
  | DAY -> 0
  | IOC -> 1

(* Build the Greeksoft-specific JSON using config + order *)
let to_greeksoft_json (config : Config.t) (order : Entities.Order.t) : Yojson.Basic.t =
  let gtoken = "101001232" (* or from config if needed *) in
  let corderid = "3"        (* ideally generated dynamically, hardcoded now *) in
  `Assoc [
    ("trigger_price", `String (string_of_float order.trigger_price));
    ("gtoken", `String gtoken);
    ("side", `String (string_of_int (side_to_int order.side)));
    ("gcid", `Int config.gcid);
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
