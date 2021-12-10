open Core

(* Rules
   1 has length 2
   2 has length 5 && has 2 digits in common with 4
   3 has length 5 && has 2 digits in common with 1
   4 has length 4
   5 has length 5 && has 3 digits in common with 4
   6 has length 6 && has 1 digit in common with 1
   7 has length 3
   8 has length 7

   9 has length 6 && has 2 digits in common with 1 
                  && has 4 digits in common with 4

   0 has length 6 && has 2 digits in common with 1 
                  && has 3 digits in common with 4
*)

module H = Hashtbl.Poly

let to_set s = s |> String.to_list |> Char.Set.of_list

let collect_1478 map =
    List.iter ~f:(fun s ->
        match Char.Set.length s with
        | 2 -> H.set map ~key:1 ~data:s
        | 4 -> H.set map ~key:4 ~data:s
        | 3 -> H.set map ~key:7 ~data:s
        | 7 -> H.set map ~key:8 ~data:s
        | _ -> ()
    )
;;

let split_and_remove_empty line ~on =
    line
    |> String.split ~on
    |> List.filter_map ~f:(fun s ->
        let s = String.strip s in
        if String.is_empty s then None
        else Some s
    )
;;

let parse_input line =
    match String.split ~on:'|' line with
    | [signals; output] -> 
        (split_and_remove_empty ~on:' ' signals |> List.map ~f:to_set, 
         split_and_remove_empty ~on:' ' output |> List.map ~f:to_set)
    | _ -> assert false
;;

let deduce_remaining map =
    let find = H.find_exn map in

    List.iter ~f:(fun s ->
        let add key = H.set map ~key ~data:s in

        let num_in_common_with num =
            Char.Set.inter (find num) s |> Char.Set.length
        in

        match Char.Set.length s with
        | 5 when num_in_common_with 1 = 2 -> add 3
        | 5 when num_in_common_with 4 = 2 -> add 2
        | 5 -> add 5

        | 6 when num_in_common_with 1 = 1 -> add 6
        | 6 when num_in_common_with 4 = 3 -> add 0
        | 6 -> add 9

        | _ -> ()
    )
;;

let decode_output alist output =
    let lookup = List.Assoc.find_exn alist ~equal:Char.Set.equal in
    List.fold output ~init:(0, 1000) ~f:(fun (total, pow) next ->
        total + lookup next * pow, pow / 10
    ) |> fst
;;

let process_line line =
    let map = H.create() in
    let signals, output = parse_input line in
    collect_1478 map signals;
    deduce_remaining map signals;
    let map = map |> H.to_alist |> List.Assoc.inverse in
    decode_output map output
;;

let _ =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.map ~f:process_line
    |> List.reduce_exn ~f:(+)
    |> Printf.printf "%d\n%!"
