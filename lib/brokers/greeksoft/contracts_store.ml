open Lwt.Infix
open Entities.Config

let loaded = ref false

type contract = {
  token : int;
  symbol : string;
  exchange : string;
}

(* parse csv line from the raw response of greeksoft getAllcontracts. *)
let parse_csv_line line =
  match String.split_on_char ',' line with
  | greek_token :: _exchange_token :: _segment :: _series :: _symbol :: _description
    :: _expiry :: _opt_type :: _strike :: _tick :: _lot :: trading_symbol :: _rest ->
    let token = int_of_string greek_token in
    (trading_symbol, token)
  | _ -> failwith ("Malformed CSV line: " ^ line)

let contracts_by_trading_symbol : (string, int) Hashtbl.t = Hashtbl.create 250000
let contracts_by_token : (int, string) Hashtbl.t = Hashtbl.create 250000


let fetch_and_store config =
  if !loaded then
    Lwt.return config
  else
    let uri = Uri.of_string "http://restapi.greeksoft.in:3333/getAllContract" in
    let headers =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
      |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
    in
    let gscid =
      match config.broker_config with
      | Greeksoft g -> g.gscid
    in
    let body_json =
      `Assoc [("request", `Assoc [("gscid", `String gscid)])]
    in
    let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string body_json) in
    Cohttp_lwt_unix.Client.post ~headers ~body uri
    >>= fun (_, body_stream) ->
    Cohttp_lwt.Body.to_string body_stream
    >>= fun csv_str ->
    let lines = String.split_on_char '\n' csv_str in
    let data_lines = List.tl lines in
    Lwt_list.iter_s (fun line ->
      match parse_csv_line line with
      | trading_symbol, token ->
        Hashtbl.replace contracts_by_trading_symbol trading_symbol token;
        Hashtbl.replace contracts_by_token token trading_symbol;
        Lwt.return_unit
      | exception _ ->
        Lwt.return_unit
    ) data_lines
    >>= fun () ->
    loaded := true;
    Lwt.return config


let get_token ~symbol =
  Hashtbl.find_opt contracts_by_trading_symbol symbol

let get_symbol ~token =
  Hashtbl.find_opt contracts_by_token token

let is_loaded () = !loaded
