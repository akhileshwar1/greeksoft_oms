(* greeksoft specific status *)

type greeksoft_status =
  | GS_Pending
  | GS_Completed
  | GS_Cancelled
  | GS_RMSRejected
  | GS_Other of string

let greeksoft_string_to_status = function
  | "Pending" -> GS_Pending
  | "Completed" -> GS_Completed
  | "Cancelled" -> GS_Cancelled
  | "RMS Rejected" -> GS_RMSRejected
  | other -> GS_Other other

(* map to om layer status *)
let om_status = function
  | GS_Pending -> Entities.Order.Pending
  | GS_Completed -> Entities.Order.Completed
  | GS_Cancelled -> Entities.Order.Cancelled
  | GS_RMSRejected -> Entities.Order.Rejected
  | GS_Other _ -> Entities.Order.Unknown
