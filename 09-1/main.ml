open Core

let input =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> fun lines ->
        let dimx = String.length (List.hd_exn lines) in
        let dimy = List.length lines in
        let map = Array.make_matrix ~dimx ~dimy 0 in
        List.iteri lines ~f:(fun y line ->
            for x=0 to dimx-1 do
                let v = x |> String.get line |> Char.to_int in
                map.(x).(y) <- v - (Char.to_int '0');
            done
        );
        map
;;

let get_neighbor map x y =
    if x < 0 || y < 0 || x >= Array.length map || y >= Array.length map.(0) then (
        Int.max_value
    ) else (
        map.(x).(y)
    )
;;

let find_low_points map =
    let risk = ref 0 in
    Array.iteri map ~f:(fun x col ->
        Array.iteri col ~f:(fun y point ->
            let left = get_neighbor map (x-1) y in 
            let right = get_neighbor map (x+1) y in
            let top = get_neighbor map x (y-1) in
            let bot = get_neighbor map x (y+1) in 
            if point < left && point < right && point < top && point < bot then (
                risk := !risk + point + 1
            )
        );
    );
    !risk

let _ = Printf.printf "Risk %d\n%!" (find_low_points input)
