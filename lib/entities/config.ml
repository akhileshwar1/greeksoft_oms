(* our config.ml that handles all broker configs *)

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

type broker_config =
  | Greeksoft of greeksoft_config
  (* More brokers can be added here *)

type t = {
  broker : string;
  session_token : string;
  user_id : int;
  broker_config : broker_config;
}

(* loads env var from the environment the process is running under *)
let get_env_or_default var_name default =
  match Sys.getenv_opt var_name with
  | Some value -> value
  | None -> default

let empty = {
  broker = "greeksoft";
  session_token = "";
  user_id = -1;
  broker_config = Greeksoft {
    iris_ip = "";
    iris_port = 0;
    heartbeat_interval = -1;
    gscid = get_env_or_default "GSCID" "DHAN";
    password = get_env_or_default "PWD" "d#1111111111";
    (* gscid = "DHAN"; *)
    (* password = "d#1111111111"; *)
    (* gscid = "DHAN2"; *)
    (* password = "a#1111111111"; *)
    (* gscid = "31500012C"; *)
    (* password = "z@1111111111"; *)
    app_id = "";
    gcid = -1;
    session_id = "";
  };
}

let with_session config ~session_token ~user_id =
  { config with session_token; user_id }

let with_iris config ~iris_ip ~iris_port ~heartbeat_interval =
  match config.broker_config with
  | Greeksoft g ->
      let g' = { g with iris_ip; iris_port; heartbeat_interval } in
      { config with broker_config = Greeksoft g' }

let with_session_id config ~session_id=
  match config.broker_config with
  | Greeksoft g ->
      let g' = { g with session_id } in
      { config with broker_config = Greeksoft g' }

let with_gcid config ~gcid =
  match config.broker_config with
  | Greeksoft g ->
      let g' = { g with gcid } in
      { config with broker_config = Greeksoft g' }
