type config = {
    cols : int;
    rows : int;
    mines : int }
;;

let standard_config = {cols = 10; rows = 10; mines = 15} ;;


(* Define the individual squares of board *)
type space = {
    mutable mined : bool;
    mutable seenb : bool;
    mutable flagged : bool;
    mutable neighbors : int }
;;


(* Board creation *)
type board = space array array ;;


(* Helper function that iterates over every space in the board (bord),
   applying a funciton argument (f) *)

let iterate_spaces bord f =
    for i = 0 to bord.cols-1 do
        for j = 0 to bord.rows-1 do
            f i j
;;


(*
   mine list that randomizes

   sp -> # of spaces
   m -> # of mines
*)

let mine_list sp m =
    let space_list = ref [] in
    while (List.length !space_list) < m do
        let n = Random.int sp in
        if not (List.mem n !space_list) then space_list := n :: !cell_list

    !space_list;;
;;



(* seed generator *)

let seed_gen =
    let t = Sys.time () in
    let x = int_of_float (t *. 2001.0) in
    Random.init(x mod 200200)
;;


