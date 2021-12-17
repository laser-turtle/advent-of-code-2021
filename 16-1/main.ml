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

let rec parse_type4 = function
    | 0::_::_::_::_ :: tl -> 0, tl
    | 1::_::_::_::_ :: tl -> parse_type4 tl
    | _ -> assert false

let rec parse_by_bits = function
    | a::b::c::d::e::f::g::h::i::j::k::l::m::n::o :: tl ->
        let sub, tl =
            [a;b;c;d;e;f;g;h;i;j;k;l;m;n;o]
            |> int_of_bits
            |> List.split_n tl
        in
        let rec loop sum = function
            | [] -> sum
            | lst -> 
                let tot, rest = parse lst in
                loop (tot+sum) rest
        in
        loop 0 sub, tl
    | _ -> assert false

and parse_nested = function
    | a::b::c::d::e::f::g::h::i::j::k :: tl ->
        let packets = int_of_bits [a;b;c;d;e;f;g;h;i;j;k] in
        let rec loop sum count tl =
            if count = 0 then sum, tl
            else 
                let tot, rest = parse tl in
                loop (tot + sum) (count-1) rest
        in
        loop 0 packets tl
    | _ -> assert false

and parse = function
    | v1::v2::v3 :: t1::t2::t3 :: bit :: tl -> 
        let version = v1*4 + v2*2 + v3 in
        let type_ = t1*4 + t2*2 + t3 in
        let sub_sum, tl =
            match type_, bit with
            | 4, _ -> parse_type4 (bit :: tl)
            | _, 0 -> parse_by_bits tl
            | _, 1 -> parse_nested tl
            | _ -> assert false
        in
        sub_sum + version, tl
    | tl -> 0, tl
;;

let _ =
    Printf.printf "\n%d\n%!" (parse input |> fst)
