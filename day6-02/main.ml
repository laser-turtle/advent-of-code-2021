open Core

let input =
    (Sys.get_argv()).(1)
    |> In_channel.read_lines
    |> String.concat
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
;;

let buckets = 
    let a = Array.create ~len:9 0 in
    List.iter input ~f:(fun fish -> a.(fish) <- a.(fish) + 1);
    a |> Array.to_list

let one_generation = function
    | _0 :: _1 :: _2 :: _3 :: _4 :: _5 :: _6 :: _7 :: [_8] ->
        [_1; _2; _3; _4; _5; _6; _7 + _0; _8; _0]

    | _ -> assert false

let _ =
    buckets
    |> Fn.apply_n_times ~n:256 one_generation
    |> List.reduce_exn ~f:(+)
    |> Printf.printf "%d"
