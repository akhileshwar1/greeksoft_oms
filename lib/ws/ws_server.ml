(* ws_server.ml *)
open Lwt.Infix
open Websocket_lwt_unix

let clients : Connected_client.t list ref = ref []

(* Function to handle a single incoming WebSocket connection.
   'incoming' is the input channel, 'outgoing' is the output channel. *)
let handle_connection (client : Connected_client.t) =
  clients := client :: !clients;
  Lwt_io.printf "Client connected. Total clients: %d" (List.length !clients) >>= fun () ->

  let rec loop () =
    Lwt.catch
      (fun () ->
        Connected_client.recv client >>= fun frame ->
        Lwt_io.printf "<- Received frame with opcode: %s\n" (Websocket.Frame.Opcode.to_string frame.opcode) >>= fun () ->
        match frame.opcode with
        | Websocket.Frame.Opcode.Text | Websocket.Frame.Opcode.Binary ->
          loop ()

        | Websocket.Frame.Opcode.Ping ->
          Lwt_io.printl "Received PING, sending PONG" >>= fun () ->
          let pong_frame = Websocket.Frame.create ~opcode:Pong () in
          Connected_client.send client pong_frame >>= fun () ->
          Lwt_io.printl "...PONG sent." >>= fun () ->
          loop ()

        | Websocket.Frame.Opcode.Close ->
          let code, reason =
            if String.length frame.content >= 2 then (
              (* Extract 16-bit code from the first two bytes (Network Byte Order) *)
              let hi = Char.code frame.content.[0] in
              let lo = Char.code frame.content.[1] in
              let code = (hi lsl 8) lor lo in
              (* Extract reason string from remaining bytes *)
              let reason = String.sub frame.content 2 (String.length frame.content - 2) in
              (Some code, reason)
            ) else (
              (* No code or reason provided in the payload *)
              (None, "")
            )
          in
          Lwt_io.printf "Connection closed by client (code: %s, reason: '%s')\n"
            (match code with Some c -> string_of_int c | None -> "none") reason >>= fun () -> (* Print extracted code and reason *)
          (* Respond with a close frame and exit the loop. *)
          (* Determine the code to send back *)
          let response_code = match code with
            | Some c -> c (* Use the code received if present *)
            | None -> 1000 (* Default to 1000 (Normal Closure) if no code received *)
          in
          Lwt.catch
            (fun () ->
              let response_frame = Websocket.Frame.close response_code in
              Connected_client.send client response_frame
            )
            (fun _ -> Lwt.return_unit)
          >>= fun () -> Lwt.return_unit

        | Websocket.Frame.Opcode.Pong ->
          Lwt_io.printl "Received PONG" >>= fun () ->
          loop ()

        | _ ->
          Lwt_io.eprintf "Warning: Received unhandled frame type: %s\n"
            (Websocket.Frame.Opcode.to_string frame.opcode) >>= fun () ->
          loop ()
      )
      (fun exn ->
        let error_msg = Printexc.to_string exn in
        Lwt_io.eprintf "Error during WebSocket read: %s\n" error_msg >>= fun () ->
        Lwt.return_unit
      )
  in

  (* Start the receiving loop for this client *)
  loop () >>= fun () ->

  clients := List.filter ((!=) client) !clients;
  Lwt_io.printf "Client disconnected. Total clients: %d" (List.length !clients) >>= fun () ->
  Lwt.return_unit

(* Function to start the WebSocket server *)
let start_server () =
  Lwt_io.printl "Starting WebSocket server on port 8081..." >>= fun () ->
  let mode = `TCP (`Port 8081) in

  Lwt_io.printl "Initializing Conduit context..." >>= fun () ->
  Conduit_lwt_unix.init () >>= fun ctx ->
  Lwt_io.printl "Conduit context initialized." >>= fun () ->
  Websocket_lwt_unix.establish_server ~ctx ~mode handle_connection

(* Function to broadcast a message to all connected clients *)
let broadcast_to_clients msg =
  let frame = Websocket.Frame.create ~content:msg () in
  Lwt_io.printf "Broadcasting message to %d clients: %s\n" (List.length !clients) msg >>= fun () ->

  let send oc =
    Lwt.catch
      (fun () ->
        Connected_client.send oc frame
      )
      (fun _ ->
        Lwt.return_unit
      )
  in

  (* Map the send function over the list of clients and join the resulting promises.
     Lwt.join waits for all the individual send promises to complete. *)
  Lwt.join ((List.map send !clients) : (unit Lwt.t) list)
