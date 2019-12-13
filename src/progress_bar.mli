module Style = Style
module Options : sig
  type t =
    { style : Style.t
    ; total_width : int option (* When not set, use the terminal width *)
    ; prefix : string
    }

  val default : t
end

include Intf.S with type options := Options.t
