open Core

let input =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> fun lines ->
        let dimy = List.length lines in
        let dimx = String.length (List.hd_exn lines) in
        let map = Array.make_matrix ~dimx ~dimy 0 in
        List.iteri lines ~f:(fun y l ->
            for x=0 to String.length l - 1 do
                let n = Char.to_int (String.get l x) - 48 in
                map.(x).(y) <- n
            done
        );
        map

module PQ = Set.Make(struct
    type t = int * int * int
    [@@deriving sexp, compare]
end)

let size_x m = Array.length m
let size_y m = Array.length m.(0)

let n = [(0, 1); (1, 0); (-1, 0); (0, -1)]

let is_valid map (x, y) =
    x >= 0 && y >= 0 && x < size_x map && y < size_y map

let path grid =
    let dist = Array.make_matrix 
        ~dimx:(size_x grid) 
        ~dimy:(size_y grid) 
        Int.max_value 
    in
    dist.(0).(0) <- 0;
    let queue = ref PQ.(singleton (0, 0, 0)) in
    while not (PQ.is_empty !queue) do
        let (_, cx, cy as cell) = PQ.min_elt_exn !queue in
        queue := PQ.remove !queue cell;

        let n = List.map n ~f:(fun (a, b) -> (cx+a,cy+b)) in
        let n = List.filter n ~f:(is_valid grid) in
        List.iter n ~f:(fun (x, y) ->
            let cost = grid.(x).(y) + dist.(cx).(cy) in
            if dist.(x).(y) > cost then (
                queue := PQ.add !queue (cost, x, y);
                dist.(x).(y) <- cost;
            )
        )
    done;
    dist.(size_x grid - 1).(size_y grid - 1)

let _ : unit = Printf.printf "%d\n%!" (path input)
