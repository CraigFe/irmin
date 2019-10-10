module type AW = sig
  include Irmin.ATOMIC_WRITE_STORE

  val v : ?fresh:bool -> ?readonly:bool -> string -> t Lwt.t

  val close : t -> unit Lwt.t
end
