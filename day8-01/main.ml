open Core

let process_line line =
    line
    |> String.split ~on:'|'
    |> List.tl_exn
    |> List.hd_exn
    |> String.split ~on:' '
    |> List.count ~f:(fun s ->
        let l = String.length s in
        l = 2 || l = 3 || l = 4 || l = 7
    )
;;

let _ =
    Sys.(get_argv()).(1)
    |> In_channel.read_lines
    |> List.map ~f:process_line
    |> List.reduce_exn ~f:(+)
    |> Printf.printf "%d\n%!"
