open Printf

let file_receitas = "receitas.csv"
let message = "Hello!"

(* let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

(* let concat_strings (a b) = a ^ b  *)

e
let read_lines file_name : string list =
  let ic = open_in file_name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []



let () = 
  let result = read_lines file_receitas in
  print_endline line;       (* write the result to stdout *)
        flush stdout *)

let rec append_lists lst1 lst2 = 
  match lst1 with 
  | [] -> lst2
  | h :: t -> h :: append_lists t lst2;;


let concat_texts txt1 txt2 = 
  txt1 ^ txt2;;


let rec insert_at_end l i =
  match l with
    [] -> [i]
  | h :: t -> h :: (insert_at_end t i)


(* let rec find_column_break string =
  match string with
  "" -> ()
  | '\t' -> print_string "OI"
  | -> find_column_break String.sub s i (String.length s - i) *)

let rec print_list = function 
[] -> ()
| e::l -> print_string e; print_string "QUEBRAA";  print_list l



(* Read the file and turns into array of lines *)
let lines_in_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
  List.rev !lines ;;



let iterate_line_by_line lines = 
  let lines_length = List.length lines in
  for i = 1 to lines_length - 1 do 
    print_string (List.nth lines i)
  done;;
  



let columns_by_line_matrix lines = 
  let my_matrix = Array.make_matrix 16 25 "" in   
  let lines_length = List.length lines in
  for i = 1 to lines_length - 1 do 
    (* Get all lines and split as columns when element is found *)
    let columns = String.split_on_char '\t' (List.nth lines i) in
      let columns_length = List.length columns in
      for j = 1 to columns_length do
        (* Get jth column and put it on the list according to line (i) and column (j) *)
        let column_nth = List.nth columns j in 
        my_matrix.(i).(j) <- column_nth
        print_string (List.nth columns j)
      done
  done;




(* Get line input by the user *)
(* Receives an array of lines and return the requested_line *)
let requested_line lines = 
  let line_from_user = read_int() in List.nth lines line_from_user;;


