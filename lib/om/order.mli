
val place_order : Entities.Config.t -> Entities.Order.t -> Entities.Order.t Lwt.t
val cancel_order : Entities.Config.t -> Entities.Order.t -> Entities.Order.t Lwt.t
val get_order_status : Entities.Config.t -> Entities.Order.t -> Entities.Order.t Lwt.t
