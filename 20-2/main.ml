open Core

let decoder, grid =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> (fun lines ->
        let decoder = List.hd_exn lines in
        Printf.printf "%s\n%!" decoder;
        assert (String.length decoder = 512);
        decoder, List.drop lines 2
    )

let grid iters =
    let dimy = List.length grid in
    let dimx = String.length (List.hd_exn grid) in
    let arr = Array.make_matrix ~dimx:(dimx + iters*2) ~dimy:(dimy + iters*2) 0 in
    List.iteri grid ~f:(fun y line ->
        for x=0 to (String.length line - 1) do
            let c = String.get line x in
            arr.(x+iters).(y+iters) <- if Char.(c = '.') then 0 else 1
        done
    );
    arr

let dx a = Array.length a.(0)
let dy a = Array.length a

let get flip a x y =
    let dx = Array.length a.(0) in
    let dy = Array.length a in
    if x < 0 || y < 0 || x >= dx || y >= dy then (if flip then 1 else 0)
    else a.(x).(y)
;;

let get_pixel flip grid x y =
      get flip grid (x+1)  (y+1)
    + get flip grid  x     (y+1) * 2
    + get flip grid (x-1)  (y+1) * 4
    + get flip grid (x+1)  y     * 8
    + get flip grid  x     y     * 16
    + get flip grid (x-1)  y     * 32
    + get flip grid (x+1) (y-1)  * 64
    + get flip grid  x    (y-1)  * 128
    + get flip grid (x-1) (y-1)  * 256
;;

let decode decoder index =
    if Char.(String.get decoder index = '#') then 1 else 0

let count_pixels arr =
    let count = ref 0 in
    for y=0 to dy arr - 1 do
        for x=0 to dy arr - 1 do
            if arr.(x).(y) = 1 then 
                incr count
        done
    done;
    !count

let enhance flip input =
    let dimx = dx input in
    let dimy = dy input in
    let out = Array.make_matrix ~dimx ~dimy 0 in
    for y=0 to dimy-1 do
        for x=0 to dimx-1 do
            let pixel = get_pixel flip input x y in
            out.(x).(y) <- decode decoder pixel
        done
    done;
    out

let print arr =
    let dimx = dx arr in
    let dimy = dy arr in
    for y=0 to dimy-1 do
        for x=0 to dimx-1 do
            if arr.(x).(y) = 0 
            then print_string "."
            else print_string "#"
        done;
        print_endline "";
    done;
    print_endline "";
    arr

let times = 50

let _ =
    (false, grid times)
    |> Fn.apply_n_times ~n:times (fun (flip, grid) -> 
        let grid = enhance flip grid in
        not flip, grid
    )
    |> snd
    |> count_pixels
    |> Printf.printf "%d\n%!"
