(* config.mli *)

type t = {
  iris_ip : string;
  iris_port : int;
  session_token : string;
  user_id : int;
  heartbeat_interval : int;
}

  val empty : t
  val with_session : t -> session_token:string -> user_id:int-> t
  val with_iris : t -> iris_ip:string -> iris_port:int -> heartbeat_interval:int -> t
