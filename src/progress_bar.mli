type t

module Style : sig
  type t =
    | Utf
    | Ascii
end

module Options : sig
  type t =
    { style : Style.t
    ; total_width : int option (* When not set, use the terminal width *)
    ; prefix : string
    }

  val default : t
end

val create : ?options:Options.t -> total:int -> unit -> t
val reset : t -> unit
val update : t -> int -> unit
val incr : t -> by:int -> unit
val close : t -> unit
val with_bar : ?options:Options.t -> total:int -> unit -> f:(t -> 'a) -> 'a
