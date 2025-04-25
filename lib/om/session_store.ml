(* Stores the latest session in memory *)

let current_session : Entities.Config.t option ref = ref None

let set (config : Entities.Config.t) : unit =
  current_session := Some config

let get () : Entities.Config.t option =
  !current_session

let clear () : unit =
  current_session := None
