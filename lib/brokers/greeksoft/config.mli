(* config.mli *)

type t = {
  iris_ip : string;
  iris_port : int;
  session_token : string;
  user_id : int;
  heartbeat_interval : int;
  gscid : string;
  password : string;
  app_id : string;
}

  val empty : t
  val with_session : t -> session_token:string -> user_id:int-> t
  val with_iris : t -> iris_ip:string -> iris_port:int -> heartbeat_interval:int -> t
  val with_app_id : t -> app_id:string -> t
