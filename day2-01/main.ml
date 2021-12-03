open Core

let input =
    "./day2-01/input.txt"
    |> In_channel.read_lines
    |> List.filter_map ~f:(fun s ->
        match String.split ~on:' ' s with
        | [a; b] -> Some (a, Int.of_string b)
        | _ -> None
    )
;;

let update (x, y) (command, amount) =
    match command with
    | "forward" -> (x + amount, y)
    | "down" -> (x, y + amount)
    | "up" -> (x, y - amount)
    | _ -> (x, y)

let result =
    let (x, y) = List.fold_left input ~init:(0, 0) ~f:update in
    x*y
;;

let _ =
    Printf.printf "Result: %d\n%!" result
