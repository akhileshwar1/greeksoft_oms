
type t = {
  token : int;
  trade_symbol : string;
  strategy_name : string;
  product_type : int;         (* Could be abstracted to a variant later *)
  net_qty : int;
  net_avg : float;
  ltp : float option;
  day_pnl : float;            (* Broker calls it DayNetAmt *)
  mtm : float;
  overall_mtm : float;
}
