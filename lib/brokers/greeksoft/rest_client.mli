(* src/rest_client.mli *)

val login : username:string -> password:string -> Config.t Lwt.t
