open Entities.Config
open Lwt.Infix

let uuid = Uuidm.v4_gen (Random.State.make_self_init ())
let generate_session_token () : string =
  Uuidm.to_string (uuid ())

let login ~username ~password ~broker=
  match broker with
  | "greeksoft" ->
      Greeksoft.Rest_client.login ~username ~password
      >>= Greeksoft.Rest_client.get_flag_values (* Rare case of passing configs to greeksoft layer *)
      >>= Greeksoft.Rest_client.get_login_info
      >>= Greeksoft.Rest_client.jlogin_new
      >>= Greeksoft.Contracts_store.fetch_and_store
      >>= Greeksoft.Rest_client.connect_to_iris
  | "dummy" ->
    Lwt.return 
      { broker = broker;
        session_token = generate_session_token ();
        user_id = 0;
        broker_config = Dummy () }
  | "zerodha" ->
    Lwt.return
      { broker = broker;
        session_token = password; (* the login process for zerodha is in the data layer, manual stuff. *)
        user_id = 0;
        broker_config = Zerodha () }
  | _ ->
      failwith "Unsupported broker"
