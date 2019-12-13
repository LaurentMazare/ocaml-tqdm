module Options : sig
  type t =
    { style : Style.t (* The width of the progress zone of the bar. *)
    ; width : int
    }

  val default : t
end

include Intf.S with type options := Options.t

