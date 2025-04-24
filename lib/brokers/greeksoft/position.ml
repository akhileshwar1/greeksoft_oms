open Yojson.Basic.Util

let from_json (json : Yojson.Basic.t) : Entities.Position.t =
  {
    token = json |> member "token" |> to_int;
    trade_symbol = json |> member "tradeSymbol" |> to_string;
    strategy_name = json |> member "StrategyName" |> to_string;
    product_type = json |> member "ProductType" |> to_int;
    net_qty = json |> member "netQty" |> to_int;
    net_avg = json |> member "netAvg" |> to_float;
    ltp = json |> member "ltp" |> to_float_option;
    day_pnl = json |> member "DayNetAmt" |> to_float;
    mtm = json |> member "MTM" |> to_float;
    overall_mtm = json |> member "OverAllMTM" |> to_float;
  }
