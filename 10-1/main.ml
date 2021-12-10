open Core

let score = function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let inverse = function
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> assert false

let calc_score line =
    let stack = Stack.create () in
    let rec loop = function
        | [] -> 0

        | ('(' as c) :: tl
        | ('{' as c) :: tl
        | ('[' as c) :: tl
        | ('<' as c) :: tl ->
            Stack.push stack (inverse c);
            loop tl

        | c :: tl when Poly.(Stack.top stack = Some c) ->
            Stack.pop stack |> ignore;
            loop tl

        | c :: _ -> score c
    in
    loop (String.to_list line)

let input =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.map ~f:calc_score
    |> List.reduce_exn ~f:(+)

let _ = Printf.printf "%d\n%!" input
