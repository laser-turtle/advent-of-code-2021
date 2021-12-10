open Core

let inverse = function
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> assert false

let score = function
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> assert false

let stack_score = 
    let rec loop total = function
        | [] -> total
        | hd :: tl -> loop (score hd + 5*total) tl
    in 
    loop 0
;;

let calc_score line =
    let rec loop = function
        | (_ :: _ as s), [] -> stack_score s
        | _, [] -> 0

        | s, ('(' as c) :: tl
        | s, ('{' as c) :: tl
        | s, ('[' as c) :: tl
        | s, ('<' as c) :: tl -> loop (inverse c :: s, tl)

        | top :: s, c :: tl when Char.(top = c) ->
            loop (s, tl)

        | _ -> 0
    in
    loop ([], String.to_list line)
;;

let input =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.map ~f:calc_score
    |> List.filter ~f:((<>) 0)
    |> List.sort ~compare:Int.compare
    |> (fun l -> List.nth_exn l (List.length l / 2))

let _ = Printf.printf "%d\n%!" input
