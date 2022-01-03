open Core

module CubeSet = Set.Make(struct
    type t = int * int * int
    [@@deriving sexp, compare]
end)

type range = {
    on : bool;
    xs : int;
    xe : int;
    ys : int;
    ye : int;
    zs : int;
    ze : int;
}

let read_coord line =
    let l = String.split line ~on:'=' in
    let l = String.split (List.nth_exn l 1) ~on:'.' in
    let n1 = Int.of_string (List.hd_exn l) in
    let n2 = Int.of_string (List.nth_exn l 2) in
    n1, n2

let read_line line =
    let s = String.split line ~on:' ' in
    let on = if String.(List.hd_exn s = "on") then true else false in
    let n = String.split ~on:',' (List.nth_exn s 1) in
    let xs, xe = read_coord (List.nth_exn n 0) in
    let ys, ye = read_coord (List.nth_exn n 1) in
    let zs, ze = read_coord (List.nth_exn n 2) in
    { on; xs; xe; ys; ye; zs; ze }

let is_valid_range r  = 
    r.xs >= -50 && r.xe <= 50
    && r.ys >= -50 && r.ye <= 50
    && r.zs >= -50 && r.ze <= 50

let apply set on coord =
    let f = 
        if on then CubeSet.add 
        else CubeSet.remove 
    in
    f set coord

let apply_range set range =
    let set = ref set in
    for x=range.xs to range.xe do
        for y=range.ys to range.ye do
            for z = range.zs to range.ze do
                set := apply !set range.on (x, y, z)
            done
        done
    done;
    !set

let () =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.map ~f:read_line
    |> List.filter ~f:is_valid_range
    |> List.fold ~init:CubeSet.empty ~f:apply_range
    |> (fun s -> Printf.printf "%d\n%!" (CubeSet.length s))
