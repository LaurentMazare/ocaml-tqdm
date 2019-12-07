open Base

let format_num f =
  Printf.sprintf "%.3g" f
  |> String.substr_replace_first ~pattern:"+0" ~with_:"+"
  |> String.substr_replace_first ~pattern:"-0" ~with_:"-"

let format_rate rate =
  match Float.classify rate with
  | Infinite | Nan | Zero -> "n/a"
  | Normal | Subnormal ->
    if Float.( < ) rate 1.
    then Printf.sprintf "s/%5.2f" (1. /. rate)
    else Printf.sprintf "%5.2f/s" rate

module Time = struct
  module Span = struct
    type t = float

    let divmod n m = n / m, n % m

    let format seconds =
      let sign, seconds =
        if Float.( < ) seconds 0. then "-", -.seconds else "", seconds
      in
      let seconds = Int.of_float seconds in
      let minutes, seconds = divmod seconds 60 in
      let hours, minutes = divmod minutes 60 in
      if hours <> 0
      then Printf.sprintf "%s%d:%0.2d:%0.2d" sign hours minutes seconds
      else Printf.sprintf "%s%0.2d:%0.2d" sign minutes seconds

    let of_secs = Fn.id
    let to_secs = Fn.id
  end

  type t = float

  let now () = Unix.gettimeofday ()
  let diff = ( -. )
end
