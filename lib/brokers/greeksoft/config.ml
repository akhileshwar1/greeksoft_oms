(* config.ml *)
type t = {
  iris_ip : string;
  iris_port : int;
  session_token : string;
  user_id : int;
  heartbeat_interval: int;
  gscid : string;
  password : string;
  app_id : string;
  gcid : int;
}

let empty = {
  iris_ip = "";
  iris_port = 0;
  session_token = "";
  user_id = -1;
  heartbeat_interval = -1;
  gscid = "DHAN";
  password = "greek@123";
  app_id = "";
  gcid = -1;
}

let with_session config ~session_token ~user_id =
  {config with session_token; user_id}

let with_iris config ~iris_ip ~iris_port ~heartbeat_interval =
  {config with iris_ip; iris_port; heartbeat_interval}

let with_app_id config ~app_id=
  {config with app_id}

let with_gcid config ~gcid=
  {config with gcid}
