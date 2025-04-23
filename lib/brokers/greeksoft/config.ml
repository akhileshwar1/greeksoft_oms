(* config.ml *)
type t = {
  iris_ip : string;
  iris_port : int;
  session_token : string;
  user_id : int;
}

let empty = {
  iris_ip = "";
  iris_port = 0;
  session_token = "";
  user_id = -1;
}

let with_session config ~session_token ~user_id =
  {config with session_token; user_id}

let with_iris config ~iris_ip ~iris_port =
  {config with iris_ip; iris_port}
