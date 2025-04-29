open Lwt.Infix
open Yojson.Basic.Util
open Entities.Config

let loaded = ref false

type contract = {
  token : int;
  symbol : string;
  exchange : string;
}

let contracts_by_symbol : (string * string, int) Hashtbl.t = Hashtbl.create 100000
let contracts_by_token : (int, string) Hashtbl.t = Hashtbl.create 100000

let fetch_and_store config =
  if !loaded then Lwt.return config else begin
  let uri = Uri.of_string "http://restapi.greeksoft.in:3333/getContracts" in
  let headers =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
    |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
  in
  let gscid=
    match config.broker_config with
    | Greeksoft g -> g.gscid
  in
  let body_json = `Assoc [("request", `Assoc [("gscid", `String gscid)])] in
  let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string body_json) in
  Cohttp_lwt_unix.Client.post ~headers ~body uri
  >>= fun (_, body_stream) ->
  Cohttp_lwt.Body.to_string body_stream
  >>= fun body_str ->
  let json = Yojson.Basic.from_string body_str in
  let data = json |> member "data" |> to_list in
  List.iter (fun item ->
    let token = item |> member "token" |> to_int in
    let symbol = item |> member "symbol" |> to_string in
    let exchange = item |> member "exchange" |> to_string in
    Hashtbl.replace contracts_by_symbol (symbol, exchange) token;
    Hashtbl.replace contracts_by_token token symbol
  ) data;
  loaded := true; (* set the ref to avoid unnecessary calls *)
  Lwt.return config 
 end

let get_token ~symbol ~exchange =
  Hashtbl.find_opt contracts_by_symbol (symbol, exchange)

let get_symbol ~token =
  Hashtbl.find_opt contracts_by_token token

let is_loaded () = !loaded
