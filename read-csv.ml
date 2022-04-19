open Printf

let file_receitas = "receitas.csv"

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


let iterate_line_by_line lines = 
  let lines_length = List.length lines in
  for i = 1 to lines_length - 1 do 
    print_string (List.nth lines i)
  done;;
  

let create_recipe_matrix lines = 
  let lines_length = List.length lines in
  let columns_length = List.length (String.split_on_char '\t' (List.nth lines 1)) in
  let recipes_ingredients_matrix = Array.make_matrix lines_length columns_length "" in   
  for i = 0 to lines_length - 1 do 
    let columns = String.split_on_char '\t' (List.nth lines i) in
    (* Get all lines and split as columns when element is found *)
      for j = 0 to columns_length - 1 do
        (* Get jth column and put it on the list according to line (i) and column (j) *)
        let column_nth = List.nth columns j in 
        recipes_ingredients_matrix.(i).(j) <- column_nth
      done
  done;
  recipes_ingredients_matrix



(* Get line input by the user *)
(* Receives an array of lines and return the requested_line *)
let requested_line lines = 
  let line_from_user = read_int() in List.nth lines line_from_user;;



(* While user input is not 0 keep asking for text *)
let () = 
  let lines_read = lines_in_file file_receitas in
  let matrix_of_ingredients = create_recipe_matrix lines_read in 
  let rec read_input input  =
    match input with
    -1 ->  ();
    | _ -> 
      print_endline "Diz ai receita que tu quer po. Se quiser parar manda um -1 pra nois"; let input_by_user = read_int() in
      print_endline (List.nth lines_read input_by_user); 
      read_input input_by_user in read_input 9;
