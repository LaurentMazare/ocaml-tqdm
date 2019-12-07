open Base

module Style = struct
  type t =
    | Default
    | Ascii

  let bars = function
    | Default -> [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]
    | Ascii -> [| " "; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "#" |]
end

module Options = struct
  type t =
    { style : Style.t
    ; width : int
    ; prefix : string
    }

  let default = { style = Default; width = 40; prefix = "" }
end

type t =
  { options : Options.t
  ; total : int
  ; bars : string array
  ; out_channel : Stdio.Out_channel.t
  ; file_descr : Unix.file_descr
  ; mutable current : int
  }

let create ?(options = Options.default) ~total () =
  { options
  ; total
  ; current = 0
  ; bars = Style.bars options.style
  ; out_channel = Stdio.stdout
  ; file_descr = Unix.stdout
  }

let update t v =
  if Unix.isatty t.file_descr
  then (
    let v = Int.max 0 (Int.min v t.total) in
    t.current <- v;
    let current_f = Float.of_int t.current in
    let total_f = Float.of_int t.total in
    let bar_len = Array.length t.bars in
    let fills = current_f /. total_f *. Float.of_int t.options.width in
    let ifills = Int.of_float fills in
    Stdio.Out_channel.output_string t.out_channel "\r ";
    Stdio.Out_channel.output_string t.out_channel t.options.prefix;
    for _i = 1 to ifills do
      Stdio.Out_channel.output_string t.out_channel t.bars.(bar_len - 1)
    done;
    if t.current <> t.total
    then (
      let i = Float.of_int bar_len *. (fills -. Float.of_int ifills) in
      Stdio.Out_channel.output_string t.out_channel t.bars.(Int.of_float i));
    for _i = 1 to t.options.width - ifills - 1 do
      Stdio.Out_channel.output_string t.out_channel t.bars.(0)
    done;
    let pct = current_f /. total_f *. 100. in
    Stdio.Out_channel.output_string t.out_channel (Printf.sprintf " [%4.1f%%]" pct);
    Stdio.Out_channel.output_char t.out_channel '\r';
    Stdio.Out_channel.flush t.out_channel)

let close t =
  update t t.total;
  Stdio.Out_channel.output_char t.out_channel '\n';
  Stdio.Out_channel.flush t.out_channel

let reset t = update t 0
let incr t ~by = update t (t.current + by)

let with_bar ?options ~total () ~f =
  let t = create ?options ~total () in
  Exn.protectx ~f t ~finally:close
