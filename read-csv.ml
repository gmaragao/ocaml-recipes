open Printf
open Format

let file_receitas = "receitas.csv"

let rec append_lists lst1 lst2 = 
  match lst1 with 
  | [] -> lst2
  | h :: t -> h :: append_lists t lst2;;


let concat_texts txt1 txt2 = 
  txt1 ^ txt2;;


let rec insert_at_end l f =
  match l with
    [] -> [f]
  | h :: t -> h :: (insert_at_end t f)


let rec print_list = function 
[] -> ()
| e::l -> print_float e; print_list l;


let split_on_first_space =
  let re = Str.regexp "[ \t\r\n]" in
  function s -> Str.bounded_split re s 2

let split_on_first_comma =
  let re = Str.regexp "[,]" in
  function s -> Str.bounded_split re s 2
  

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
  print_endline ("Nome: " ^ recipes_matrix.(recipe_number).(1));

let print_recipe_preparation recipe_number recipes_matrix = 
  print_endline "Preparo: ";
  print_string recipes_matrix.(recipe_number).(1);

let rec print_all_recipes_titles recipes_matrix line_number = 
  match line_number with
  | _ when line_number >= Array.length recipes_matrix -> ()
  | _ -> 
    print_int line_number; print_string " - ";
    print_recipe_title line_number recipes_matrix;
    print_endline "******";
    print_all_recipes_titles recipes_matrix (line_number+1)
    

let rec print_n_recipes_details recipe_number recipes_matrix remaining_recipes =
  match remaining_recipes with
  0 -> ()
  | _ -> 
    print_endline "Escolha 3 receitas das citadas."; let input_by_user = read_int() in
    print_ingredients_for_recipe input_by_user matrix_of_ingredients;
    read_input input_by_user in read_input 9;


type ingredient_detail = {
  name: string;
  total: float;
  unit: string;
}


(* let find_equal_ingredients recipes_matrix recipe_number1 recipe_number2 = 
  let equal_ingredients_quantity = ref [] in 
  let equal_ingredients_name = ref [] in 
  let equal_ingredients_unit = ref [] in
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
              equal_ingredients_name := ingredient_name :: !equal_ingredients_name;

              let str = recipes_matrix.(recipe_number1).(j+1) in
              match split_on_first_space str with
              | [first; rest] ->
                let quantity_number_string_1 = first in
                let quantity_unit_1 = rest in

              let str_2 = recipes_matrix.(recipe_number2).(k+1) in
              match split_on_first_space str_2 with
              | [first_2; rest_2] ->
                let quantity_number_string_2 = first_2 in
                let quantity_unit_2 = rest_2 in

                let quantity_number_float_1 = float_of_string quantity_number_string_1 in
                let quantity_number_float_2 = float_of_string quantity_number_string_2 in

                let tuple = (quantity_number_float_1, quantity_number_float_2) in 
                equal_ingredients_quantity := tuple :: !equal_ingredients_quantity;

                if quantity_unit_1 = quantity_unit_2 then 
                  equal_ingredients_unit :=  (quantity_unit_1, "") :: !equal_ingredients_unit
                else equal_ingredients_unit := (quantity_unit_1, quantity_unit_2) ::  !equal_ingredients_unit;
            );
          get_other_ingredient (k+2))
      in get_other_ingredient 3; get_ingredient (j+2)) 
      in get_ingredient 3; 
      let ingredients_list_dict = (List.rev !equal_ingredients_name, List.rev !equal_ingredients_quantity, List.rev !equal_ingredients_unit) in
      (* Returns *) 
      ingredients_list_dict; *)



let find_equal_ingredients recipes_matrix recipe_number1 recipe_number2 = 
  let ingredients_list = ref [] in 
  let rec get_ingredient j =
    (match j with
    |  _ when j >= Array.length recipes_matrix.(recipe_number1) - 1 -> ()
    |  _ ->
      let ingredient_name = recipes_matrix.(recipe_number1).(j) in
      let rec get_other_ingredient k =
        (match k with
        | _ when k >= Array.length recipes_matrix.(recipe_number2)  - 1 -> ()
        | _ when recipes_matrix.(recipe_number2).(k) = "" -> get_other_ingredient (k+2)
        | _ ->
          let other_ingredient = recipes_matrix.(recipe_number2).(k) in
          if ingredient_name = other_ingredient then 
          (
              (* equal_ingredients_name := ingredient_name :: !equal_ingredients_name; *)
              let str = recipes_matrix.(recipe_number1).(j+1) in
                match split_on_first_space str with 
                | [] -> () 
                | [first; rest] ->
                  let quantity_number_string_1 = first in
                  let quantity_unit_1 = rest in

              let str_2 = recipes_matrix.(recipe_number2).(k+1) in
                match split_on_first_space str_2 with
                | [] -> () 
                | [first_2; rest_2] ->
                  let quantity_number_string_2 = first_2 in
                  let quantity_unit_2 = rest_2 in
                if quantity_unit_1 = quantity_unit_2 then (
                  let quantity_number_float_1 = float_of_string quantity_number_string_1 in
                  let quantity_number_float_2 = float_of_string quantity_number_string_2 in
                  let qty_total = quantity_number_float_1 +. quantity_number_float_2 in 
                  let ing_dict = {
                    name = ingredient_name; 
                    total = qty_total; 
                    unit = quantity_unit_1
                  }; in 
                  ingredients_list := ing_dict :: !ingredients_list
                  (* print_endline ing_dict; *)
                )
               
                (* let tuple = (quantity_number_float_1, quantity_number_float_2) in  *)
                (* equal_ingredients_quantity := tuple :: !equal_ingredients_quantity; *)

                (* if quantity_unit_1 = quantity_unit_2 then 
                  equal_ingredients_unit :=  (quantity_unit_1, "") :: !equal_ingredients_unit
                else equal_ingredients_unit := (quantity_unit_1, quantity_unit_2) ::  !equal_ingredients_unit; *)
          );
          get_other_ingredient (k+2))
      in get_other_ingredient 3; get_ingredient (j+2)) 
      in get_ingredient 3; 
      (* let ingredients_list_dict = (List.rev !equal_ingredients_name, List.rev !equal_ingredients_quantity, List.rev !equal_ingredients_unit) in *)
      (* Returns *) 
      List.rev !ingredients_list;

(* let sum_equal_ingredients equal_ingredients_1, equal_ingredients_2, equal_ingredients_3 = *)

let get_all_equal_ingredients recipes_matrix recipe1 recipe2 recipe3 = 
  let all_equals = ref [] in 
  let equal_ingredients_1_2 = find_equal_ingredients recipes_matrix recipe1 recipe2 in
  let equal_ingredients_1_3 = find_equal_ingredients recipes_matrix recipe1 recipe3 in
  let equal_ingredients_2_3 = find_equal_ingredients recipes_matrix recipe2 recipe3 in
  


let all_ingredients_sum recipes_matrix recipe1 recipe2 recipe3 = 
  let final_ings_qty = ref [] in 
  let final_ings_qty_updated = ref [] in
  let all_ings_1 = get_all_ingredients recipes_matrix recipe1 in
  let all_ings_2 =  get_all_ingredients recipes_matrix recipe2 in
  let all_ings_3 = get_all_ingredients recipes_matrix recipe3 in
  for i = 0 to (List.length all_ings_1) - 1 do 
    let ing_list_1 = List.nth all_ings_1 i in
    for j = 0 to (List.length all_ings_2) - 1 do
      let ing_list_2 = List.nth all_ings_2 j in
      if (ing_list_1.name = ing_list_2.name && ing_list_1.unit = ing_list_2.unit) then (
        let ings_sum = ing_list_1.total +. ing_list_2.total in
        let ingredient = {
          name = ing_list_1.name;
          total = ings_sum;
          unit = ing_list_1.unit;
        } in 
        final_ings_qty := ingredient :: !final_ings_qty;
      )
      else if (
        ing_list_1.name = ing_list_2.name
      ) then (
        let ingredient = {
          name = ing_list_1.name;
          total = ing_list_1.total;
          unit = ing_list_1.unit;
        } in 
        final_ings_qty := ingredient :: !final_ings_qty;

        let ingredient = {
          name = ing_list_2.name;
          total = ing_list_2.total;
          unit = ing_list_2.unit;
        } in 
        final_ings_qty := ingredient :: !final_ings_qty;
      )
    done;
    let new_list = ref [] in 
    for k = 0 to (List.length !final_ings_qty) - 1 do
      let ing_in_final_list = List.nth !final_ings_qty k in
      if (ing_list_1.name = ing_in_final_list.name && ing_list_1.unit = ing_in_final_list.unit) then (
        new_list := ing_list_1.name :: !new_list
      )
    done;
    if List.length !new_list = 0 then (
      print_endline "ADICIONA SE NAO FOR IGUAL";
       let ingredient = {
          name = ing_list_1.name;
          total = ing_list_1.total;
          unit = ing_list_1.unit;
        } in 
      final_ings_qty := ingredient :: !final_ings_qty;
    )
  done;
  for y = 0 to (List.length all_ings_2) -1 do
    let ing_list_2 = List.nth all_ings_2 y in
    let new_list = ref [] in 
    for k = 0 to (List.length !final_ings_qty) - 1 do
      let ing_in_final_list = List.nth !final_ings_qty k in
      if (ing_list_2.name = ing_in_final_list.name && ing_list_2.unit = ing_in_final_list.unit) then (
        new_list := ing_list_2.name :: !new_list
      )
    done;
    if List.length !new_list = 0 then (
      print_endline "ADICIONA SE NAO FOR IGUAL";
       let ingredient = {
          name = ing_list_2.name;
          total = ing_list_2.total;
          unit = ing_list_2.unit;
        } in 
      final_ings_qty := ingredient :: !final_ings_qty;
    )
  done;
  (* Checks if elements is list 3 are in the final ings_sum
  if not add them to new list
  if are sum and add to new list
  check the ones remaing from first final ings *)
   for i = 0 to (List.length all_ings_3) - 1 do 
    let ing_list_3 = List.nth all_ings_3 i in
    for j = 0 to (List.length !final_ings_qty) - 1 do
      let final_ings_list = List.nth !final_ings_qty j in
      if (ing_list_3.name = final_ings_list.name && ing_list_3.unit = final_ings_list.unit) then (
        let ings_sum = ing_list_3.total +. final_ings_list.total in
        let ingredient = {
          name = ing_list_3.name;
          total = ings_sum;
          unit = ing_list_3.unit;
        } in 
        final_ings_qty_updated := ingredient :: !final_ings_qty_updated;
      )
      else if (
        ing_list_3.name = final_ings_list.name
      ) then (
        let ingredient = {
          name = ing_list_3.name;
          total = ing_list_3.total;
          unit = ing_list_3.unit;
        } in 
        final_ings_qty_updated := ingredient :: !final_ings_qty_updated;

        let ingredient = {
          name = final_ings_list.name;
          total = final_ings_list.total;
          unit = final_ings_list.unit;
        } in 
        final_ings_qty_updated := ingredient :: !final_ings_qty_updated;
      )
    done;
    let new_list = ref [] in 
    for k = 0 to (List.length !final_ings_qty_updated) - 1 do
      let ing_in_final_list = List.nth !final_ings_qty_updated k in
      if (ing_list_3.name = ing_in_final_list.name && ing_list_3.unit = ing_in_final_list.unit) then (
        new_list := ing_list_3.name :: !new_list
      )
    done;
    if List.length !new_list = 0 then (
      print_endline "ADICIONA SE NAO FOR IGUAL";
       let ingredient = {
          name = ing_list_3.name;
          total = ing_list_3.total;
          unit = ing_list_3.unit;
        } in 
      final_ings_qty_updated := ingredient :: !final_ings_qty_updated;
    )
  done;
  for y = 0 to (List.length !final_ings_qty) -1 do
    let final_ings_list = List.nth !final_ings_qty y in
    let new_list = ref [] in 
    for k = 0 to (List.length !final_ings_qty_updated) - 1 do
      let ing_in_final_list = List.nth !final_ings_qty_updated k in
      if (final_ings_list.name = ing_in_final_list.name && final_ings_list.unit = ing_in_final_list.unit) then (
        new_list := final_ings_list.name :: !new_list
      )
    done;
    if List.length !new_list = 0 then (
      print_endline "ADICIONA SE NAO FOR IGUAL";
       let ingredient = {
          name = final_ings_list.name;
          total = final_ings_list.total;
          unit = final_ings_list.unit;
        } in 
      final_ings_qty_updated := ingredient :: !final_ings_qty_updated;
    )
  done;
  List.rev !final_ings_qty_updated;




let get_all_ingredients recipes_matrix recipe_number =
  let ingredients_list = ref [] in
  let rec get_ingredient i =
  (match i with 
  | _ when i >=  Array.length recipes_matrix.(recipe_number) - 1 -> ();
  | _ -> 
  (* print_int i; *)
  let ingredient_name = recipes_matrix.(recipe_number).(i) in
  (* print_int (Array.length recipes_matrix.(recipe_number)); *)
  let ing_detail_str = recipes_matrix.(recipe_number).(i+1) in
    (match ing_detail_str with
    | "" -> print_endline "ACABA POR FAVBOR"; ();
    | _ ->  
      (* print_endline ("AQUI: " ^ ing_detail_str); *)
      if ing_detail_str = "" then (print_endline "ACABA AQUI AGORA"; ();)
      else (
        (* print_endline "ENTROU NO ELSE"; *)
        let is_int = Str.string_match (Str.regexp "[0-9]+$") ing_detail_str 0 in
        let is_float_with_dot = Str.string_match (Str.regexp "[0-9].[0-9]+$") ing_detail_str 0 in
        let is_float_with_comma = Str.string_match (Str.regexp "[0-9],[0-9]+$") ing_detail_str 0 in

        (* print_endline ("String: " ^ing_detail_str); *)
        (* print_endline "BOOL: ";
        printf "%b " is_int;
        printf "%b " is_float_with_dot;
        printf "%b" is_float_with_comma;
        print_endline ""; *)

        if (is_int  || is_float_with_dot || is_float_with_comma) then (
          let ingredient = {
            name = ingredient_name;
            total = float_of_string ing_detail_str;
            unit = "unidades";
          } in 
          print_endline "é numero";
          ingredients_list := ingredient :: !ingredients_list;
          get_ingredient (i+2)
        ) 
        else (
          (* print_endline "ENTROU"; *)
          (match split_on_first_space ing_detail_str with
          | [] -> ()
          | [first; rest] ->
            print_endline ("FIRST: " ^ first ^ "REST: " ^  rest);
            print_endline ingredient_name;
            let quantity_number_string = first in
            let is_number_first = Str.string_match (Str.regexp "[0-9]+$") first 0 in
            let is_float_with_dot_first = Str.string_match (Str.regexp "[0-9].[0-9]+$") first 0 in 
            let is_float_with_comma_first = Str.string_match (Str.regexp "[0-9],[0-9]+$") first 0 in
            printf "%b " is_number_first;
            printf "%b " is_float_with_dot_first;
            printf "%b" is_float_with_comma_first;
            if (is_number_first || is_float_with_dot_first || is_float_with_comma_first) then (
              print_endline "TA AQUI ?";
              let quantity_unit = rest in
              if is_float_with_comma_first then (
                match split_on_first_comma first with
                | [] -> ()
                | [first; rest] ->
                  let new_float_string = first ^ "." ^ rest in 
                  print_endline ("new float stirng: " ^ new_float_string);
                  let quantity_number_float = float_of_string new_float_string in
                  let ingredient = {
                    name = ingredient_name;
                    total = quantity_number_float;
                    unit = quantity_unit;
                  } in 
                ingredients_list := ingredient :: !ingredients_list;
                get_ingredient (i+2)
              ) else (
                let quantity_number_float = float_of_string first in 
                print_endline ingredient_name;
                print_endline "deve aparecer aqui!";
                let ingredient = {
                  name = ingredient_name;
                  total = quantity_number_float;
                  unit = quantity_unit;
                } in 
                ingredients_list := ingredient :: !ingredients_list;
                get_ingredient (i+2)
              )
            )
            else if (first = "a") then (
              let ingredient = {
                name = ingredient_name;
                total = 0.;
                unit = first ^ " " ^ rest;
              } in 
              ingredients_list := ingredient :: !ingredients_list;
              get_ingredient (i+2);
            )
            else (
              (* print_endline "ENTROU NESSE ELSE AQUI"; *)
              (* print_string ("UNIT: " ^ quantity_number_string); *)
              let ingredient = {
                name = ingredient_name;
                total = 0.;
                unit = quantity_number_string;
              } in 
          
              ingredients_list := ingredient :: !ingredients_list;
              get_ingredient (i+2);
            )
          )
        )
      )
    )
  )
    in 
      get_ingredient 3;
      List.rev !ingredients_list;



let print_all_ingredients_from_lists recipes_matrix recipe1 recipe2 recipe3 =
  let final_ingredients_text = ref [] in 
  let all_equal_ings = get_all_equal_ingredients recipes_matrix recipe1 recipe2 recipe3 in 
  let print_ingredients_for_recipe recipe_number recipes_matrix =
  print_endline "Ingredientes: ";
  (* Getting only the ingredients *)
  for i = 3 to 24 do
    let ing =  recipes_matrix.(recipe_number).(i) in
    let rec dicts_list_in_list i = 
      match i with
      | _ when i >= List.length dicts_list_in_list -> ()
      | _ -> 
        let dicts_list = List.nth dicts_list_in_list in
        let rec dict_list j = 
        match j with
        | _ when j >= List.length dict_list -> ()
        | _ -> 
        let ing_name = List.nth dicts_list j in
        if ing_name = ing then
        final_ingredients_text = []
        
    in ing_in_equals 0
  done;
  
  
  


let get_all_equals list1 list2 =
let list_of_equals = ref [] in 
let rec get_all_equals_l1 i =
  (match i with
  | _ when i > List.length list1 -> ()
  | _ ->  
    let element_list_1 = List.nth list1 i in
    let rec get_all_equals_l2 j = 
    (match j with
      | _ when j > List.length list2 -> ()
      | _ -> 
      let element_list_2 = List.nth list2 j in 
      if element_list_1.name = element_list_2.name then 
        let el_total = element_list_1.total in
        let el_total_2 = element_list_2.total in 
        let total_els = el_total +. el_total_2 in print_float total_els;
        let final_equals = {
          name = element_list_1.name;
          total = total_els;
          unit = element_list_1.unit
        }; in 
        list_of_equals := final_equals :: !list_of_equals;
      
    get_all_equals_l2 (j+1))
    in get_all_equals_l2 0; get_all_equals_l1 (i+1))
    in get_all_equals_l1 0;
    List.rev !list_of_equals;


let choose_option number recipes_matrix =
  match number with
  | 1 -> print_all_recipes_titles recipes_matrix 1;
  | 2 ->
      print_endline "Diga o número da receita."; 
      let input_by_user = read_int() in
      print_ingredients_for_recipe input_by_user recipes_matrix
  | 3 -> print_endline "VISH";


 (* in 
      let print_recipe number = print_recipe_preparation recipe_number recipes_matrix in
      print_recipe recipe_number; *)

(* print_string recipes_matrix.(recipe_number).(5);
While user input is not 0 keep asking for text *)
 let () = 
  let lines_read =   file_receitas in
  let recipes_matrix = create_recipe_matrix lines_read in 
  let rec read_input input =
    match input with
    0 ->  ();
    | _ -> 
      print_endline "*******************************************";
      print_endline "Digite o número da ação desejada e pressione 'ENTER' ";
      print_endline "1 - Listar receitas";
      print_endline "2 - Ver detalhes da receita"; 
      print_endline "3 - Escolher 3 receitas e calcular total"; 
      print_endline "0 - Finalizar processo"; 
      print_endline "Aragão Inc. ®";
      print_endline "******************************************";
      let input_by_user = read_int() in
      choose_option input_by_user recipes_matrix; 
      read_input input_by_user in read_input 9;


(* 
let () = 
  let lines_read = lines_in_file file_receitas in
  let matrix_of_ingredients = create_recipe_matrix lines_read in 
  print_all_recipes_titles matrix_of_ingredients 1; *)


        print_endline "Diz ai receita que tu quer po. Se quiser parar manda um -1 pra nois"; let input_by_user = read_int() in
      print_ingredients_for_recipe input_by_user matrix_of_ingredients;
      read_input input_by_user in read_input 9;

      