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
| e::l -> print_endline e; print_list l;



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
  List.rev !lines;


let iterate_line_by_line lines = 
  let lines_length = List.length lines in
  for i = 1 to lines_length - 1 do 
    print_string (List.nth lines i)
  done;
  

let create_recipe_matrix lines = 
  let lines_length = List.length lines in
  let columns_length = List.length (String.split_on_char '\t' (List.nth lines 1)) in
  let recipes_matrix = Array.make_matrix lines_length columns_length "" in   
  for i = 0 to lines_length - 1 do 
    let columns = String.split_on_char '\t' (List.nth lines i) in
    (* Get all lines and split as columns when element is found *)
      for j = 0 to columns_length - 1 do
        (* Get jth column and put it on the list according to line (i) and column (j) *)
        let column_nth = List.nth columns j in 
        recipes_matrix.(i).(j) <- column_nth
      done
  done;
  recipes_matrix;


let print_ingredients_for_recipe recipe_number recipes_matrix =
  print_endline "Ingredientes: ";
  (* Getting only the ingredients *)
  for i = 3 to 24 do
    print_string recipes_matrix.(recipe_number).(i); print_string " ";
  done;
  

let print_recipe_title recipe_number recipes_matrix =
  print_string ("Nome: " ^ recipes_matrix.(recipe_number).(1));

let print_recipe_preparation recipe_number recipes_matrix = 
  print_endline "Preparo: ";
  print_string recipes_matrix.(recipe_number).(1);

let rec print_all_recipes_titles recipes_matrix line_number = 
  match line_number with
  | _ when line_number >= Array.length recipes_matrix -> ()
  | _ -> 
    print_recipe_title line_number recipes_matrix;
    print_all_recipes_titles recipes_matrix (line_number+1)
    

let rec print_n_recipes_details recipe_number recipes_matrix remaining_recipes =
  match remaining_recipes with
  0 -> ()
  | _ -> 
    print_endline "Escolha 3 receitas das citadas."; let input_by_user = read_int() in
    print_ingredients_for_recipe input_by_user matrix_of_ingredients;
    read_input input_by_user in read_input 9;

    
let find_equal_ingredients recipes_matrix recipe_number1 recipe_number2 = 
  let rec get_ingredient j =
    (match j with
    |  _ when j >= Array.length recipes_matrix.(recipe_number1) - 1 -> ()
    |  _ ->
      let ingredient_name = recipes_matrix.(recipe_number1).(j) in
      let rec get_other_ingredient k =
        (match k with
        | _ when k >= Array.length recipes_matrix.(recipe_number2) -> ()
        | _ when recipes_matrix.(recipe_number2).(k) = "" -> get_other_ingredient (k+2)
        | _ ->
          let other_ingredient = recipes_matrix.(recipe_number2).(k) in
          if ingredient_name = other_ingredient then 
            (
              print_endline (ingredient_name ^ " " ^ other_ingredient); 
              print_endline (recipes_matrix.(recipe_number2).(j+1) ^ " " ^ recipes_matrix.(recipe_number2).(k+1));
              let quantity_number = String.split_on_char ' ' recipes_matrix.(recipe_number2).(k+1) in print_endline (List.nth quantity_number 0);
            );
          get_other_ingredient (k+2))
      in get_other_ingredient 3; get_ingredient (j+2)) 
      in get_ingredient 3; 
        (* )  *)

          (* print_string recipes_matrix.(recipe_number).(5);
(* While user input is not 0 keep asking for text *)
(* let () = 
  let lines_read = lines_in_file file_receitas in
  let matrix_of_ingredients = create_recipe_matrix lines_read in 
  let rec read_input input  =
    match input with
    -1 ->  ();
    | _ -> 
      print_endline "Diz ai receita que tu quer po. Se quiser parar manda um -1 pra nois"; let input_by_user = read_int() in
      print_ingredients_for_recipe input_by_user matrix_of_ingredients;
      read_input input_by_user in read_input 9;
 *)


let () = 
  let lines_read = lines_in_file file_receitas in
  let matrix_of_ingredients = create_recipe_matrix lines_read in 
  print_all_recipes_titles matrix_of_ingredients 1;