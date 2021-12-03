open Core

let input =
    "./day2-02/input.txt"
    |> In_channel.read_lines
    |> List.filter_map ~f:(fun s ->
        match String.split ~on:' ' s with
        | [a; b] -> Some (a, Int.of_string b)
        | _ -> None
    )

let update (x, y, aim) (command, amount) =
    match command with
    | "forward" -> (x + amount, y + aim*amount, aim)
    | "down" -> (x, y, aim + amount)
    | "up" -> (x, y, aim - amount)
    | _ -> (x, y, aim)

let result =
    let (x, y, _) = List.fold_left input ~init:(0, 0, 0) ~f:update in
    x*y

let _ =
    Printf.printf "Result: %d\n%!" result
