open Core
module H = Hashtbl.Poly

let neighbors = [
    (1, 0); (0, 1); (-1, 0); (0, -1);
    (1, 1); (-1, 1); (1, -1); (-1, -1);
]

let distribute map (x, y) =
    H.set map ~key:(x, y) ~data:~-1;
    List.iter neighbors ~f:(fun (dx, dy) ->
        H.change map (x+dx, y+dy) ~f:(function
            | Some v when v >= 0 && v <= 9 -> Some (v+1)
            | v -> v
        )
    )

let gather_flashes =
    H.fold ~init:[] ~f:(fun ~key:loc ~data:energy acc ->
        if energy > 9 then (
            loc :: acc
        ) else acc
    )

let step map =
    H.map_inplace map ~f:((+) 1);

    let rec loop count =
        let updates = gather_flashes map in
        List.iter updates ~f:(distribute map);
        let u = List.length updates in
        if u = 0 then () 
        else loop (count + u)
    in

    loop 0;
    H.map_inplace map ~f:(max 0);
;;

let all_flashed = H.for_all ~f:((=) 0)

let rec loop count map =
    if all_flashed map then 
        count
    else (
        step map;
        loop (count+1) map
    )
;;

let _ =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.mapi ~f:(fun y s ->
        s
        |> String.to_list
        |> List.mapi ~f:(fun x c -> (x, y, c))
    )
    |> List.join
    |> List.fold ~init:(H.create()) ~f:(fun map (x, y, c) ->
        H.set map ~key:(x, y) ~data:(Char.to_int c - 48);
        map
    )
    |> loop 0 
    |> Printf.printf "%d\n%!"
