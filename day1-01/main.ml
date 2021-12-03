open Core

let input =
    "./day1-01/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:int_of_string
;;

let distances = 
    match input with
    | [] | [_]  -> 0
    | hd :: tl ->
        let rec count total prev = function
            | [] -> total
            | next :: rest ->
                let total = 
                    if next > prev 
                    then total+1 
                    else total
                in
                count total next rest
        in
        count 0 hd tl
;;

let _ =
    Printf.printf "Total increases: %d\n%!" distances
