type t

val create : ?width:int -> total:int -> unit -> t
val reset : t -> unit
val update : t -> int -> unit
val incr : t -> by:int -> unit
val close : t -> unit
val with_bar : ?width:int -> total:int -> unit -> f:(t -> 'a) -> 'a
