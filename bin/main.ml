(* bin/main.ml *)

open Lwt.Infix

let () =
  let username = "DHAN" in
  let password = "greek@123" in
  Lwt_main.run (
    Om.Session.login ~username ~password
    >>= fun config ->
    let dummy_order =
      {
        Entities.Order.tradingsymbol = "GRASIM";
        exchange = "NSE";
        quantity = 1;
        price = 1.0;
        trigger_price = 0.0;
        side = Entities.Order.Buy;
        order_type = Entities.Order.Limit;
        product = Entities.Order.CNC;
        validity = Entities.Order.DAY;
        strategy_name = Some "Glitters_bb";
        broker_order_id = Some "";
        status = Entities.Order.Pending;
      } in
    Om.Order.place_order config dummy_order
    >>= fun updated_order ->
    Lwt_io.printf "Order placed with broker_order_id: %s\n"
      (Option.value ~default:"<none>" updated_order.broker_order_id)
    >>= fun () ->
    Om.Order.cancel_order config updated_order
    >>= fun _ ->
    Lwt_io.printf "Order status after cancellation: \n"
  )
