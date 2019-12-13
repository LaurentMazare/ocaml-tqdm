(* Faster version of the progress bar. *)
open Base

module Options = struct
  type t =
    { style : Style.t
    ; width : int
    }

  let default = { style = Utf; width = 40 }
end

type t =
  { options : Options.t
  ; total : int
  ; bars : string array
  ; out_channel : Stdio.Out_channel.t
  ; isatty : bool
  ; mutable current : int
  ; mutable start_time : Utils.Time.t
  }

let create ?(options = Options.default) total =
  { options
  ; start_time = Utils.Time.now ()
  ; total
  ; current = 0
  ; bars = Style.bars options.style
  ; out_channel = Stdio.stdout
  ; isatty = Unix.isatty Unix.stdout
  }

let update t v =
  let v = Int.max 0 (Int.min v t.total) in
  t.current <- v;
  if t.isatty
  then (
    let elapsed = Utils.Time.(diff (now ()) t.start_time) in
    (* TODO: add EMA ? *)
    let rate = Float.of_int t.current /. Utils.Time.Span.to_secs elapsed in
    let remaining =
      Float.of_int (t.total - t.current) /. rate |> Utils.Time.Span.of_secs
    in
    let current_f = Float.of_int t.current in
    let total_f = Float.of_int t.total in
    let bar_len = Array.length t.bars in
    let fills = current_f /. total_f *. Float.of_int t.options.width in
    let ifills = Int.of_float fills in
    let pct = current_f /. total_f *. 100. in
    Stdio.Out_channel.output_char t.out_channel '\r';
    Stdio.Out_channel.output_char t.out_channel ' ';
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
    Stdio.Out_channel.fprintf
      t.out_channel
      "|%3.0f%% [%4d/%4d | %s | %s<%s] "
      pct
      t.current
      t.total
      (Utils.format_rate rate)
      (Utils.Time.Span.format elapsed)
      (Utils.Time.Span.format remaining);
    Stdio.Out_channel.flush t.out_channel)

let close t =
  Stdio.Out_channel.output_char t.out_channel '\n';
  Stdio.Out_channel.flush t.out_channel

let reset t =
  t.start_time <- Utils.Time.now ();
  update t 0

let incr t ~by = update t (t.current + by)

let with_bar ?options total ~f =
  let t = create ?options total in
  Exn.protectx ~f t ~finally:close
