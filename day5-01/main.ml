open Core

let list_to_tuple = function
    | [a;b] -> (a, b)
    | _ -> assert false
;;

let input =
    "./day5-01/input"
    |> In_channel.read_lines
    |> List.map ~f:(fun line ->
        let parts = String.split ~on:' ' line in
        let start = String.split ~on:',' (List.hd_exn parts) in
        let end_  = String.split ~on:',' (List.nth_exn parts 2) in
        let start = List.map ~f:Int.of_string start |> list_to_tuple in
        let end_  = List.map ~f:Int.of_string end_  |> list_to_tuple in
        start, end_
    )
;; 

let calc_increment a b =
    let d = b - a in
    if d = 0 then 0
    else if d > 0 then 1
    else -1
;;

let count =
    let xmax, ymax =
        List.fold input ~init:(0, 0) ~f:(fun (xmax, ymax) ((x1, y1), (x2, y2)) ->
            let x = max x1 x2 in
            let y = max y1 y2 in
            max xmax (x+1), max ymax (y+1)
        )
    in
    let map = Array.make_matrix ~dimx:xmax ~dimy:ymax 0 in
    List.iter input ~f:(fun ((x1, y1), (x2, y2)) ->
        let dx = calc_increment x1 x2 in
        let dy = calc_increment y1 y2 in
        if (dx = 0 && dy <> 0) || (dx <> 0 && dy = 0) then (
            let x = ref x1 in
            let y = ref y1 in
            while !x <> (x2+dx) || !y <> (y2+dy) do
                map.(!x).(!y) <- map.(!x).(!y) + 1;
                x := !x + dx;
                y := !y + dy;
            done
        );
    );

    let overlap_count = ref 0 in
    Array.iter map ~f:(fun col ->
        Array.iter col ~f:(fun value ->
            if value > 1 then 
                incr overlap_count
        )
    );

    !overlap_count
;;

let _ = Printf.printf "Overlapping lines %d\n%!" count
