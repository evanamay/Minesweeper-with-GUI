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


(* determines if a space (x,y) in a board (brd) is valid
   also for neighbors *)

let valid (x,y) brd = 
    x>=0 && x<brd.cols && y>=0 && y<brd.rows
;;

let neighbor (x,y) brd = 
    let neighborspace = [(x-1,y-1); (x-1,y); (x-1,y+1); (x,y-1); (x,y+1); (x+1,y-1); (x+1,y); (x+1,y+1)] in
    List.filter (valid brd) neighborspace
;;

let create_board brd = 
   let create_space () = { mined=false; seen=false; flagged=false; neighbors=0 } in 
   let copy_space b (x,y) = b.(x).(y) <- create_space() in
   let set_mine_flag s n = s.(n / brd.rows).(n mod brd.rows).mined <- true in
   let neighbor_mines b (x,y) =
     let x = ref 0 in
     let mine_count (x,y) = if b.(x).(y).mined then incr x 
     in List.iter mine_count (neighbor brd (x,y)) ;
        !x
   in
   let set_count b (x,y) =
     if not b.(x).(y).mined 
     then b.(x).(y).neighbors <- set_mine_flag b (x,y)
   in
   let list_mined = mine_list (brd.cols*brd.rows) brd.mines in 
   let board = Array.make_matrix brd.cols brd.rows (create_space()) 
   in iterate_spaces brd (copy_space board) ;
      List.iter (set_mine_flag board) list_mined ;
      iterate_spaces brd (set_count board) ;
      board
;;
