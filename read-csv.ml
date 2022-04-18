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


(* Get line input by the user *)
(* Receives an array of lines and return the requested_line *)
let requested_line lines = 
  let line_from_user = read_int() in List.nth lines line_from_user;;


