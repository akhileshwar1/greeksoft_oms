(* src/rest_client.mli *)

val login : username:string -> password:string -> Entities.Config.t Lwt.t
val get_flag_values : Entities.Config.t -> Entities.Config.t Lwt.t
val get_login_info : Entities.Config.t -> Entities.Config.t Lwt.t
val jlogin_new : Entities.Config.t -> Entities.Config.t Lwt.t
val place_order : headers:Cohttp.Header.t -> body:Yojson.Basic.t -> string Lwt.t
val cancel_order : headers:Cohttp.Header.t -> order_id:string -> string Lwt.t
