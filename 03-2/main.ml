open Core

let input =
    "./day3-02/input.txt"
    |> In_channel.read_lines
    |> List.map ~f:String.to_array
;;

let ( = ) = Char.equal

let num_ones column =
    List.count ~f:(fun row -> row.(column) = '1')
;;

let to_num arr = 
    Array.fold_right arr ~init:(0, 1) ~f:(fun ch (num, pow) ->
        let num = if ch = '1' then num + pow else num in
        (num, pow*2)
    ) |> fst
;;

let filter_list pred lst =
    let rec loop column = function
        | [] -> assert false

        | [value] -> to_num value

        | lst ->
            let len = List.length lst in
            let ones = num_ones column lst in 
            let ones = pred ones len in

            let lst = List.filter lst ~f:(fun str ->
                (ones && str.(column) = '1') 
                || (not ones && str.(column) = '0')
            ) in

            loop (column+1) lst
    in
    loop 0 lst
;;

let oxygen_num =
    filter_list (fun ones len -> ones >= (len - ones)) input

let co2_num =
    filter_list (fun ones len -> ones < (len - ones)) input

let _ =
    Printf.printf "Result: %d %d = %d\n%!" oxygen_num co2_num (oxygen_num * co2_num)
