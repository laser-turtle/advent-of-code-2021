open Core

let input =
    "./day1-02/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:int_of_string
;;

let distances = function
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

let collapse lst =
    let rec loop acc = function
        | [] -> acc
        | a :: [] -> a :: acc
        | a :: b :: [] -> (a+b) :: acc
        | a :: b :: c :: tl -> loop ((a + b + c) :: acc) (b :: c :: tl)
    in
    loop [] lst |> List.rev
;;

let _ =
    Printf.printf "Total increases: %d\n%!" (distances (collapse input))
