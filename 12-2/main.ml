open Core
module H = Hashtbl.Poly

let graph =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.map ~f:(String.split ~on:'-')
    |> List.fold ~init:(H.create()) ~f:(fun map (lst : string list)->
        match lst with
        | [from; to_] -> 
            if String.(from <> "end" && to_ <> "start") then (
                H.update map from ~f:(function
                    | None -> String.Set.singleton to_
                    | Some set -> String.Set.add set to_
                );
            );
            if String.(from <> "start" && to_ <> "end") then (
                H.update map to_ ~f:(function
                    | None -> String.Set.singleton from
                    | Some set -> String.Set.add set from
                );
            );
            map
        | _ -> assert false
    )

module SLS = Set.Make(struct
    type t = string list
    [@@deriving compare, sexp]
end)

let is_lower = String.for_all ~f:Char.is_lowercase

let rec find_paths map paths double_visited full_path smalls node =
    let in_small_set = String.Set.mem smalls node in
    if String.(node = "end") then (
        paths := SLS.add !paths full_path
    ) else if 
        (in_small_set && not double_visited) || (not in_small_set) 
    then (
        let double_visited = in_small_set || double_visited in
        let edges = H.find_exn map node in
        let smalls =
            if is_lower node then  
                String.Set.add smalls node
            else smalls 
        in
        Set.iter edges ~f:(fun edge ->
            find_paths map paths double_visited (edge :: full_path) smalls edge
        )
    )
;;

let _ : unit =
    let set = ref SLS.empty in
    find_paths graph set false [] String.Set.empty "start";
    Printf.printf "count %d\n%!" (SLS.length !set)
