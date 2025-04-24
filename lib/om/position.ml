open Lwt.Infix

let get_position_by_token (config : Entities.Config.t) (token : int) : Entities.Position.t Lwt.t =
  match config.broker with
  | "greeksoft" ->
    Greeksoft.Rest_client.get_strategy_positions config
    >>= fun json_list ->
    let positions =
      List.map Greeksoft.Position.from_json json_list
    in
    begin match List.find_opt (fun p -> p.token = token) positions with
      | Some pos -> Lwt.return pos
      | None -> failwith ("Token not found: " ^ string_of_int token)
      end
  | _ ->
    failwith "Unsupported broker"
