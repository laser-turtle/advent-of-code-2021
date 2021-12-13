open Core
module H = Hashtbl.Poly

let paper = H.create()

type fold = X of int
          | Y of int

let folds =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.split_while ~f:(Fn.compose not String.is_empty)
    |> (fun (coords, folds) ->
            coords
            |> List.map ~f:String.(split ~on:',')
            |> List.iter ~f:(function
                | [x;y] -> 
                    let x = Int.of_string x
                    and y = Int.of_string y in
                    H.set paper ~key:(x, y) ~data:()
                | _ -> assert false 
            );
            folds
            |> List.tl_exn
            |> List.map ~f:String.(split ~on:'=')
            |> List.map ~f:(function
                | [dir; num] ->
                    let num = Int.of_string num in
                    if Char.(String.get dir (String.length dir - 1) = 'x') then (
                        X num
                    ) else Y num
                | _ -> assert false
            )
    )

let do_fold v fold =
    if v > fold then 
        fold - abs (v - fold) 
    else v

let fold paper dir =
    let f = H.create() in
    H.iteri paper ~f:(fun ~key:(x, y) ~data:_ ->
        let key =
            match dir with 
            | X fold -> (do_fold x fold, y)
            | Y fold -> (x, do_fold y fold)
        in
        H.set f ~key ~data:()
    );
    f
;;

let print paper =
    let dimx, dimy = H.fold paper ~init:(0, 0) 
        ~f:(fun ~key:(x, y) ~data:_ (xmax, ymax) ->
            max xmax (x+1), max ymax (y+1)
    ) in
    let a = Array.make_matrix ~dimx ~dimy ' ' in
    H.iter_keys paper ~f:(fun (x, y) ->
        a.(x).(y) <- '#'
    );
    for y=0 to dimy-1 do
        for x=0 to dimx-1 do
            Printf.printf "%c" a.(x).(y)
        done;
        print_endline ""
    done
;;

let _ =
    folds
    |> List.fold ~init:paper ~f:fold
    |> print
