open Core

module H = Hashtbl.Poly

let input =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> fun lines ->
        let dimx = String.length (List.hd_exn lines) in
        let map = H.create() in
        List.iteri lines ~f:(fun y line ->
            for x=0 to dimx-1 do
                let v = (x |> String.get line |> Char.to_int) - Char.to_int '0' in
                if v < 9 then (
                    H.set map ~key:(x, y) ~data:v
                )
            done
        );
        map
;;

let get_neighbor map x y =
    match H.find map (x, y) with
    | None -> Int.max_value
    | Some v -> v
;;

let find_low_points map =
    let seeds = ref [] in
    H.iteri map ~f:(fun ~key:(x, y) ~data:point ->
        let left = get_neighbor map (x-1) y in 
        let right = get_neighbor map (x+1) y in
        let top = get_neighbor map x (y-1) in
        let bot = get_neighbor map x (y+1) in 
        if point < left && point < right && point < top && point < bot then (
            seeds := (x, y) :: !seeds
        )
    );
    !seeds
;;

let rec region_size map (x, y as loc) =
    H.remove map loc;

    let check x y =
        match H.find map (x, y) with
        | None -> 0
        | Some _ -> region_size map (x, y)
    in

    1 
    + check (x-1) y 
    + check (x+1) y
    + check x (y-1)
    + check x (y+1)
;;

let _ =
    input
    |> find_low_points
    |> List.map ~f:(region_size input)
    |> List.sort ~compare:Int.descending
    |> (fun l -> List.take l 3)
    |> List.reduce_exn ~f:( * )
    |> Printf.printf "%d\n%!"

