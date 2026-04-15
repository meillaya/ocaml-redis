module type SOURCE = sig
  type t

  val now_ms : t -> int64
end

module System = struct
  type t = unit

  let create () = ()
  let now_ms () = Unix.gettimeofday () |> ( *. ) 1000. |> Int64.of_float
end

module Manual = struct
  type t = { mutable now_ms : int64 }

  let create ~now_ms = { now_ms }
  let now_ms clock = clock.now_ms
  let set_ms clock now_ms = clock.now_ms <- now_ms

  let advance_ms clock delta_ms =
    clock.now_ms <- Int64.add clock.now_ms delta_ms
end
