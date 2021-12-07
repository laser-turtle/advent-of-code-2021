open Core

let input =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> String.concat
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
;;

let fuel lst =
    let arr = Array.of_list lst in
    Array.sort arr ~compare:Int.compare;
    let median = arr.(Array.length arr / 2) in
    Array.fold arr ~init:0 ~f:(fun total value ->
        total + abs (value - median)
    )

let _ =
    Printf.printf "%d\n%!" (fuel input)
