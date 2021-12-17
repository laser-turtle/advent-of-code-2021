open Core

let rec to_bin_list = function
    | '0' :: tl -> [0;0;0;0] @ to_bin_list tl
    | '1' :: tl -> [0;0;0;1] @ to_bin_list tl
    | '2' :: tl -> [0;0;1;0] @ to_bin_list tl
    | '3' :: tl -> [0;0;1;1] @ to_bin_list tl
    | '4' :: tl -> [0;1;0;0] @ to_bin_list tl
    | '5' :: tl -> [0;1;0;1] @ to_bin_list tl
    | '6' :: tl -> [0;1;1;0] @ to_bin_list tl
    | '7' :: tl -> [0;1;1;1] @ to_bin_list tl
    | '8' :: tl -> [1;0;0;0] @ to_bin_list tl
    | '9' :: tl -> [1;0;0;1] @ to_bin_list tl
    | 'A' :: tl -> [1;0;1;0] @ to_bin_list tl
    | 'B' :: tl -> [1;0;1;1] @ to_bin_list tl
    | 'C' :: tl -> [1;1;0;0] @ to_bin_list tl
    | 'D' :: tl -> [1;1;0;1] @ to_bin_list tl
    | 'E' :: tl -> [1;1;1;0] @ to_bin_list tl
    | 'F' :: tl -> [1;1;1;1] @ to_bin_list tl
    | [] -> []
    | _ -> assert false

let input =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.hd_exn
    |> String.to_list
    |> to_bin_list

let int_of_bits lst =
    lst
    |> List.map ~f:Int.to_string
    |> String.concat
    |> (fun s -> "0b" ^ s)
    |> Int.of_string

type exp = Lit of int
         | Sum of exp list
         | Prod of exp list
         | Min of exp list
         | Max of exp list
         | Gt of exp list
         | Lt of exp list
         | Eq of exp list

let parse_type4 lst = 
    let rec loop acc = function
        | 0::a::b::c::d :: tl -> acc @ [a;b;c;d], tl
        | 1::a::b::c::d :: tl -> loop (acc @ [a;b;c;d]) tl
        | _ -> assert false
    in
    let acc, tl = loop [] lst in
    [Lit (int_of_bits acc)], tl

let rec parse_by_bits = function
    | a::b::c::d::e::f::g::h::i::j::k::l::m::n::o :: tl ->
        let sub, tl =
            [a;b;c;d;e;f;g;h;i;j;k;l;m;n;o]
            |> int_of_bits
            |> List.split_n tl
        in
        let rec loop acc = function
            | [] -> acc
            | lst -> 
                let exp, rest = parse lst in
                loop (exp @ acc) rest
        in
        loop [] sub, tl
    | _ -> assert false

and parse_nested = function
    | a::b::c::d::e::f::g::h::i::j::k :: tl ->
        let packets = int_of_bits [a;b;c;d;e;f;g;h;i;j;k] in
        let rec loop acc count tl =
            if count = 0 then acc, tl
            else 
                let exp, rest = parse tl in
                loop (exp @ acc) (count-1) rest
        in
        loop [] packets tl
    | _ -> assert false

and parse = function
    | _::_::_:: t1::t2::t3 :: bit :: tl -> 
        let type_ = t1*4 + t2*2 + t3 in
        let exp, tl =
            match type_, bit with
            | 4, _ -> parse_type4 (bit :: tl)
            | _, 0 -> parse_by_bits tl
            | _, 1 -> parse_nested tl
            | _ -> assert false
        in
        let e = 
            match type_ with
            | 0 -> Sum exp
            | 1 -> Prod exp
            | 2 -> Min exp
            | 3 -> Max exp
            | 5 -> Gt exp
            | 6 -> Lt exp
            | 7 -> Eq exp
            | 4 -> List.hd_exn exp
            | _ -> assert false
        in
        [e], tl
    | tl -> [], tl
;;

let rec eval = 
    let recur op lst = 
        lst
        |> List.map ~f:eval 
        |> List.reduce_exn ~f:op
    in
    function
    | Lit e -> e
    | Sum lst -> recur ( + ) lst
    | Prod lst -> recur ( * ) lst
    | Min lst -> recur min lst
    | Max lst -> recur max lst
    | Lt [b;a] -> if eval a < eval b then 1 else 0
    | Gt [b;a] -> if eval a > eval b then 1 else 0
    | Eq [b;a] -> if eval a = eval b then 1 else 0
    | _ -> assert false

let _ =
    let exp, _ = parse input in
    Printf.printf "%d\n%!" (eval (List.hd_exn exp))
