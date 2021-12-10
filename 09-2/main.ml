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

let rec find_basins acc map =
    match H.choose map with
    | None -> acc
    | Some (loc, _) -> 
        find_basins (region_size map loc :: acc) map
;;

let _ =
    input
    |> find_basins []
    |> List.sort ~compare:Int.descending
    |> (fun l -> List.take l 3)
    |> List.reduce_exn ~f:( * )
    |> Printf.printf "%d\n%!"

