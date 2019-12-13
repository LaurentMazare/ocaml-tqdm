type t =
  | Utf
  | Ascii
  | Line
  | Circle
  | Braille
  | Braille_spin
  | Vertical

val bars : t -> string array
