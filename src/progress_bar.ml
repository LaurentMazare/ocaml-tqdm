open Base

let bars = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" |]

type t =
  { width : int
  ; total : int
  ; out_channel : Stdio.Out_channel.t
  ; mutable current : int
  }

let create ?(width = 40) ~total () =
  { width; total; current = 0; out_channel = Stdio.stdout }

let close t =
  Stdio.Out_channel.output_char t.out_channel '\n';
  Stdio.Out_channel.flush t.out_channel

let update t v =
  (* TODO: Check if the output is a tty and do nothing if this is
     not the case. *)
  t.current <- v;
  let current_f = Float.of_int t.current in
  let total_f = Float.of_int t.total in
  let bar_len = Array.length bars in
  let fills = current_f /. total_f *. Float.of_int t.width in
  let ifills = Int.of_float fills in
  Stdio.Out_channel.output_string t.out_channel "\r ";
  for _i = 1 to ifills do
    Stdio.Out_channel.output_string t.out_channel bars.(bar_len - 1)
  done;
  if t.current <> t.total
  then (
    let i = Float.of_int bar_len *. (fills -. Float.of_int ifills) in
    Stdio.Out_channel.output_string t.out_channel bars.(Int.of_float i));
  for _i = 1 to t.width - ifills - 1 do
    Stdio.Out_channel.output_string t.out_channel bars.(0)
  done;
  let pct = current_f /. total_f *. 100. in
  Stdio.Out_channel.printf " %4.1f%%" pct;
  Stdio.Out_channel.flush t.out_channel

let reset t = update t 0
let incr t ~by = update t (t.current + by)

let with_bar ?width ~total () ~f =
  let t = create ?width ~total () in
  Exn.protectx ~f t ~finally:close
