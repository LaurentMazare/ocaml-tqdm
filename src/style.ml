type t =
  | Utf
  | Ascii
  | Line
  | Circle
  | Braille
  | Braille_spin
  | Vertical

let bars = function
  | Utf -> [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]
  | Ascii -> [| " "; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "#" |]
  | Line -> [| "─"; "─"; "─"; "╾"; "╾"; "╾"; "╾"; "━"; "═" |]
  | Circle -> [| " "; "◓"; "◑"; "◒"; "◐"; "◓"; "◑"; "◒"; "#" |]
  | Braille -> [| " "; "⡀"; "⡄"; "⡆"; "⡇"; "⡏"; "⡟"; "⡿"; "⣿" |]
  | Braille_spin -> [| " "; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠇"; "⠿" |]
  | Vertical -> [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█"; "█" |]