open Core

let input =
    "./day3-01/input.txt"
    |> In_channel.read_lines
;;

let result =
    let col_counts =
        match input with
        | hd :: _ -> Array.create ~len:(String.length hd) 0
        | _ -> assert false
    in

    List.iter input ~f:(fun row ->
        for i=0 to String.length row - 1 do
            match String.get row i with
            | '1' -> col_counts.(i) <- col_counts.(i) + 1
            | _ -> ()
        done
    );

    let total_lines = List.length input in

    let cols = Array.map col_counts ~f:(fun count ->
        count > total_lines/2
    ) in

    let gamma = ref 0 in
    let epsilon = ref 0 in
    let pow = ref 1 in

    for i=(Array.length cols - 1) downto 0 do
        if cols.(i) then (
            gamma := !gamma + !pow;
        ) else (
            epsilon := !epsilon + !pow;
        );

        pow := !pow * 2;
    done;

    !gamma * !epsilon
;;

let _ =
    Printf.printf "Result: %d\n%!" result
