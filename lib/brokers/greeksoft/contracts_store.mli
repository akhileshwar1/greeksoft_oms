type contract = {
  token : int;
  symbol : string;
  exchange : string;
}

  val fetch_and_store : Entities.Config.t -> Entities.Config.t Lwt.t
  val get_token : symbol:string -> int option
  val get_symbol : token:int -> string option
  val is_loaded : unit -> bool
