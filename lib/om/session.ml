open Entities.Config
open Lwt.Infix

let login ~username ~password =
  match empty.broker with
  | "greeksoft" ->
      Greeksoft.Rest_client.login ~username ~password
      >>= Greeksoft.Rest_client.get_flag_values (* Rare case of passing configs to greeksoft layer *)
      >>= Greeksoft.Rest_client.get_login_info
      >>= Greeksoft.Rest_client.jlogin_new
      >>= Greeksoft.Contracts_store.fetch_and_store
  | _ ->
      failwith "Unsupported broker"
