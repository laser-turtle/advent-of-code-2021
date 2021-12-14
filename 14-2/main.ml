open Core
module H = Hashtbl.Poly

let rules = H.create()

let update_count map key def =
    H.update map key ~f:(function
        | None -> def
        | Some v -> v + def
    )

let input, counts =
    match 
        Sys.(get_argv()).(1)
        |> In_channel.read_lines
    with
    | input :: _ :: rule_lines ->
        let pairs = H.create() in
        let counts = H.create() in
        for i=0 to String.length input-2 do
            let p = String.sub input ~pos:i ~len:2 in
            update_count pairs p 1
        done;
        String.iter input ~f:(fun elt ->
            update_count counts (Char.to_string elt) 1
        );
        List.iter rule_lines ~f:(fun line ->
            match String.split ~on:' ' line with
            | [left; _; right] -> H.set rules ~key:left ~data:right
            | _ -> assert false
        );
        pairs, counts
    | _ -> assert false

let sub (map, counts) =
    let out = H.create() in
    H.iteri map ~f:(fun ~key ~(data : int) ->
        let res = H.find_exn rules key in
        let p1 = Char.to_string (String.get key 0) ^ res in
        let p2 = res ^ Char.to_string (String.get key 1) in
        update_count counts res data;
        update_count out p1 data;
        update_count out p2 data;
    );
    (out, counts)

let calc (_, counts) =
    H.fold counts 
        ~init:(Int.max_value, Int.min_value) 
        ~f:(fun ~key:_ ~data (min_v, max_v) ->
            min data min_v, max data max_v
    )

let _ =
    let min, max = calc (Fn.apply_n_times ~n:40 sub (input, counts)) in
    Printf.printf "%d\n%!" (max - min)
