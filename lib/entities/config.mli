type greeksoft_config = {
  iris_ip : string;
  iris_port : int;
  heartbeat_interval : int;
  gscid : string;
  password : string;
  app_id : string;
  gcid : int;
  session_id : string;
}

type dummy_config = unit

type zerodha_config = unit

type broker_config =
  | Greeksoft of greeksoft_config
  | Dummy of dummy_config
  | Zerodha of zerodha_config

type t = {
  broker : string;
  session_token : string;
  user_id : int;
  broker_config : broker_config;
}

val empty : t

val with_session :
  t -> session_token:string -> user_id:int -> t

val with_iris :
  t -> iris_ip:string -> iris_port:int -> heartbeat_interval:int -> t

val with_session_id:
  t -> session_id:string -> t

val with_gcid :
  t -> gcid:int -> t

val get_env_or_default :
  string -> string -> string
