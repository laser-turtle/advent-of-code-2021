open Core

let input =
    "./day6-01/input"
    |> In_channel.read_lines
    |> String.concat
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
;;

let one_generation =
    let rec loop acc = function
        | [] -> acc
        | hd :: tl ->
            if hd = 0 then (
                loop (6 :: 8 :: acc) tl
            ) else
                loop (hd-1 :: acc) tl
    in
    loop []

let _ =
    input
    |> Fn.apply_n_times ~n:80 one_generation
    |> List.length
    |> Printf.printf "%d"
