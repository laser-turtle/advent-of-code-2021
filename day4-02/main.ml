open Core

module Board : sig 
    type t

    val mark : t -> num:int -> unit
    val sum_unmarked : t -> int
    val is_winner : t -> bool
    val of_list : int list list -> t
end = struct
    type mark = Marked
              | Unmarked

    let rows = 5
    let cols = 5

    type row = (mark * int) array

    (* 5x5 boards *)
    type t = row array

    let mark (t : t) ~(num : int) : unit =
        Array.iter t ~f:(fun row ->
            Array.iteri row ~f:(fun x col ->
                if Poly.equal col (Unmarked, num) then (
                    row.(x) <- Marked, num
                )
            )
        )
    ;;

    let sum_unmarked (t : t) : int =
        let total = ref 0 in
        Array.iter t ~f:(fun row ->
            Array.iter row ~f:(function
                | Marked, _ -> ()
                | Unmarked, num -> total := !total + num
            )
        );
        !total
    ;;

    let is_row_all_marked : row -> bool =
        Array.for_all ~f:(function 
            | Marked, _ -> true 
            | Unmarked, _ -> false
        )
    ;;

    let is_col_all_marked (col : int) : t -> bool =
        Array.for_all ~f:(fun row ->
            match row.(col) with
            | Marked, _ -> true
            | Unmarked, _ -> false
        )
    ;;

    let is_winner (t : t) : bool =
        let row_wins = Array.exists t ~f:is_row_all_marked in
        if row_wins then true
        else (
            let rec loop col =
                if col >= cols then false
                else if is_col_all_marked col t then true
                else loop (col+1)
            in
            loop 0
        )
    ;;

    let of_list (board : int list list) : t =
        let t = Array.make_matrix ~dimx:cols ~dimy:rows (Unmarked, 0) in
        List.iteri board ~f:(fun y row ->
            List.iteri row ~f:(fun x col ->
                t.(y).(x) <- Unmarked, col
            )
        );
        t
    ;;
end

module Game : sig
    type t

    val read_input : string -> t
    val find_last_winner : t -> (int * Board.t) option
end = struct
    type t = {
        mutable boards : Board.t list;
        numbers : int list;
    }

    let string_to_numbers (s : string) : int list =
        s
        |> String.split ~on:' '
        |> List.map ~f:String.strip
        |> List.filter ~f:(Fn.compose not String.is_empty)
        |> List.map ~f:Int.of_string
    ;;

    let board_of_string_list (lst : string list) =
        lst
        |> List.map ~f:string_to_numbers
        |> Board.of_list
    ;;

    let read_input filename =
        let lines =
            filename
            |> In_channel.read_lines
        in

        let numbers = 
            lines
            |> List.hd_exn 
            |> String.split ~on:','
            |> List.map ~f:Int.of_string
        in

        let boards =
            lines
            (* Drop input numbers *)
            |> List.tl_exn
            (* Group into empty line + board groups *)
            |> List.chunks_of ~length:6
            (* Drop empty line *)
            |> List.map ~f:List.tl_exn
            (* Turn 5x5 grid into boards *)
            |> List.map ~f:board_of_string_list
        in

        { boards; numbers }
    ;;

    let find_last_winner (t : t) : (int * Board.t) option =
        let last_winner = ref None in

        List.iter t.numbers ~f:(fun num ->
            t.boards <- 
                List.filter t.boards ~f:(fun board -> 
                    Board.mark ~num board;
                    let won = Board.is_winner board in
                    if won then (
                        last_winner := Some (num, board);
                    );
                    not won
                );
        );
        !last_winner
    ;;
end

let _ = 
    "./day4-02/input.txt"
    |> Game.read_input
    |> Game.find_last_winner
    |> function
       | None -> print_endline "No winner?"
       | Some (num, board) ->
           let sum = Board.sum_unmarked board in
           Printf.printf "Winner %d %d = %d\n%!"
               num
               sum
               (num * sum)

