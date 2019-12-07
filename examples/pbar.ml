module T = Tqdm.Tqdm

let () =
  Stdio.printf "Starting pbar...\n%!";
  T.with_bar 100 ~f:(fun tqdm ->
      for v = 1 to 100 do
        Unix.sleepf 0.1;
        T.update tqdm v
      done);
  T.with_bar ~options:{ T.Options.default with style = Ascii } 100 ~f:(fun tqdm ->
      for v = 1 to 100 do
        Unix.sleepf 0.1;
        T.update tqdm v
      done)
