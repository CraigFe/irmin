module type S = sig
  type t

  exception RO_Not_Allowed

  val v : fresh:bool -> version:string -> readonly:bool -> string -> t

  val name : t -> string

  val clear : t -> unit

  val append : t -> string -> unit

  val set : t -> off:int64 -> string -> unit

  val read : t -> off:int64 -> bytes -> int

  val offset : t -> int64

  val force_offset : t -> int64

  val readonly : t -> bool

  val version : t -> string

  val sync : t -> unit
end
