open Lwt.Infix
open Entities.Config

let loaded = ref false

type contract = {
  token : int;
  data_symbol : string;
  trading_symbol: string;
}

(* parse csv line from the raw response of greeksoft getAllcontracts. *)
let parse_csv_line line =
  let columns = String.split_on_char ',' line in
  let num_columns = List.length columns in
  if num_columns < 13 then
    failwith ("Malformed CSV line (expected at least 13 columns, got " ^ (string_of_int num_columns) ^ "): " ^ line)
  else
    match columns with
    | [] -> failwith "Malformed CSV line (empty line after splitting)" (* Should not happen with >= 13 columns *)
    | greek_token_str :: _ ->
      (* Attempt to convert token string to int, handle potential errors *)
      let token =
        try
          int_of_string (String.trim greek_token_str)
        with
          | Failure _ -> failwith ("Invalid integer for token: " ^ greek_token_str)
      in

      let reversed_columns = List.rev columns in

      (* Extract the last column for the data symbol (lookup key) *)
      let data_symbol =
        match reversed_columns with
        | last :: _ -> String.trim last
        | [] -> failwith "Malformed CSV line (could not get last column)" (* Should not happen *)
      in

      (* Extract the last two columns for the trading symbol (order symbol) *)
      let trading_symbol =
        match reversed_columns with
        | last :: second_last :: _ -> (String.trim second_last) ^ "," ^ (String.trim last) (* Concatenate with comma *)
        | _ -> failwith "Malformed CSV line (could not get last two columns)" (* Should not happen if num_columns >= 13 *)
      in

      (* Printf.printf "processed line token %d, data_symbol %s, trading_symbol %s\n%!" *)
        (* token data_symbol trading_symbol; *)

      (* Return the data symbol (as key) and the new contract record *)
      (data_symbol, { token; data_symbol = data_symbol; trading_symbol = trading_symbol })

let contracts_by_data_symbol : (string, contract) Hashtbl.t = Hashtbl.create 250000
let contracts_by_token : (int, contract) Hashtbl.t = Hashtbl.create 250000

let decompress_gzip (compressed_str : string) : string =
  (* Convert input string directly to Bigstringaf.t *)
  let input_bigstring = Bigstringaf.of_string compressed_str ~off:0 ~len:(String.length compressed_str) in
  let input_len = Bigstringaf.length input_bigstring in
  let input_pos = ref 0 in

  (* Refill function: provides data to the decompressor *)
  let refill buffer =
    let len = min (input_len - !input_pos) (Bigstringaf.length buffer) in
    if len > 0 then (
      (* Use Bigstringaf.blit to copy from Bigstringaf.t to Bigstringaf.t *)
      Bigstringaf.blit input_bigstring ~src_off:!input_pos buffer ~dst_off:0 ~len;
      input_pos := !input_pos + len;
    );
    len
  in

  let output_buffer = Buffer.create 4096 in

  (* Flush function: handles the decompressed output *)
  let flush buffer len =
    let chunk = Bigstringaf.substring buffer ~off:0 ~len in
    Buffer.add_string output_buffer chunk
  in

  (* Allocate internal buffers for decompress *)
  let inflate_input_buffer = Bigstringaf.create De.io_buffer_size in
  let inflate_output_buffer = Bigstringaf.create De.io_buffer_size in

  match Gz.Higher.uncompress
    ~refill:(fun buf -> refill buf) (* Pass the refill function *)
    ~flush:(fun buf len -> flush buf len) (* Pass the flush function *)
    inflate_input_buffer (* Input buffer for decompress's internal use *)
    inflate_output_buffer (* Output buffer for decompress's internal use *)
  with
  | Ok _metadata -> Buffer.contents output_buffer (* Decompression successful, return accumulated output *)
  | Error (`Msg msg) -> failwith ("Gzip decompression failed: " ^ msg)

let fetch_and_store config =
  if !loaded then
    Lwt.return config
  else
    let uri = Uri.of_string "http://restapi.greeksoft.in:3333/getAllContract" in
    let headers =
      Cohttp.Header.init ()
      |> fun h -> Cohttp.Header.add h "Content-Type" "application/json"
      |> fun h -> Cohttp.Header.add h "Authorization" config.session_token
      |> fun h -> Cohttp.Header.add h "Accept-Encoding" "identity"
    in
    Cohttp_lwt_unix.Client.get ~headers uri
    >>= fun (_, body_stream) ->
    Cohttp_lwt.Body.to_string body_stream
    >>= fun compressed_str ->
    let csv_str = decompress_gzip compressed_str in
    (* Printf.printf "csv str is: %s\n" csv_str;  *)
    let lines = String.split_on_char '\n' csv_str in
    let data_lines = List.tl lines in
    let len = List.length data_lines in
    Printf.printf "Number of data lines: %d\n" len; 
    Lwt_list.iter_s (fun line ->
      match parse_csv_line line with
      | data_key, contract_record ->
        Hashtbl.replace contracts_by_data_symbol data_key contract_record;
        Hashtbl.replace contracts_by_token contract_record.token contract_record;
        Lwt.return_unit
      | exception _ ->
        Lwt.return_unit
    ) data_lines
    >>= fun () ->
    Printf.printf "All lines processed.\n%!";
    loaded := true;
    Lwt.return config


let get_token ~symbol =
  let result = Hashtbl.find_opt contracts_by_data_symbol symbol in
  match result with
    | Some contract -> Some contract.token 
    | None -> Some 0 
  
let get_symbol ~token =
  (* Printf.printf "Looking up contract for token: %d\n%!" token; *)
  let result = Hashtbl.find_opt contracts_by_token token in
  match result with
    | Some contract -> Some contract.trading_symbol
    | None -> Some ""

let is_loaded () = !loaded
