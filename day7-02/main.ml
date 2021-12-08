open Core

let input =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> String.concat
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
;;

let calculate_cost point =
    Array.fold ~init:0 ~f:(fun total ship ->
        let steps = abs (ship - point) in
        total + ((steps * (steps + 1)) / 2)
    )
;;

let fuel lst =
    let arr = Array.of_list lst in
    Array.sort arr ~compare:Int.compare;
    let best = ref Int.max_value in
    for point=arr.(0) to Array.last arr do
        best := min !best (calculate_cost point arr)
    done;
    !best

let _ =
    Printf.printf "%d\n%!" (fuel input)
