module type SOURCE = sig
  type t

  val now_ms : t -> int64
end

module System : sig
  type t

  val create : unit -> t
  val now_ms : t -> int64
end

module Manual : sig
  type t

  val create : now_ms:int64 -> t
  val now_ms : t -> int64
  val set_ms : t -> int64 -> unit
  val advance_ms : t -> int64 -> unit
end
