open Core
module H = Hashtbl.Poly

let paper = H.create()

let fold_x = 655
let fold_y = 7

let graph =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.map ~f:String.(split ~on:',')
    |> List.iter ~f:(function
        | [x;y] -> 
            let x = Int.of_string x
            and y = Int.of_string y in
            H.set paper ~key:(x, y) ~data:()
        | _ -> assert false 
    )

let folded_x =
    let f = H.create() in
    H.iteri paper ~f:(fun ~key:(x, y) ~data:_ ->
        if x > fold_x then (
            let new_x = fold_x - abs (x - fold_x) in
            H.set f ~key:(new_x, y) ~data:()
        ) else if x = fold_x then ()
        else (
            H.set f ~key:(x, y) ~data:()
        )
    );
    f

let folded_y =
    let f = H.create() in
    H.iteri paper ~f:(fun ~key:(x, y) ~data:_ ->
        if y >= fold_y then (
            let new_y = fold_y - abs (y - fold_y) in
            H.set f ~key:(x, new_y) ~data:()
        ) else (
            H.set f ~key:(x, y) ~data:()
        )
    );
    f

let _ = Printf.printf "X %d\n%!" (H.length folded_x)
let _ = Printf.printf "Y %d\n%!" (H.length folded_y)
