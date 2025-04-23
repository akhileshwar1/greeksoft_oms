(* src/rest_client.mli *)

val login : username:string -> password:string -> Config.t Lwt.t
val get_flag_values : Config.t -> Config.t Lwt.t
