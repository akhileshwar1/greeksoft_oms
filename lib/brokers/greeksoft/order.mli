(* Broker-specific order status type *)

type greeksoft_status =
  | GS_Pending
  | GS_Completed
  | GS_Cancelled
  | GS_RMSRejected
  | GS_Other of string

(* Parse a Greeksoft-specific status string into the broker-specific type *)
val greeksoft_string_to_status : string -> greeksoft_status

(* Map the Greeksoft-specific status to the generic OMS-wide status type *)
val om_status : greeksoft_status -> Entities.Order.status_type
