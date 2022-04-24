open Printf
open Format
open String
open Array

type ingredient_detail = {
  name: string;
  total: float;
  unit: string;
}


let file_receitas = "receitas.csv";;

let split_on_first_space =
  let re = Str.regexp "[ \t\r\n]" in
  function s -> Str.bounded_split re s 2;;

let split_on_first_comma =
  let re = Str.regexp "[,]" in
  function s -> Str.bounded_split re s 2;;


(* Read the file and turns into array of lines *)
let read_file filename = 
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines;;


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
      done;
  done;
  recipes_matrix;;


let print_ingredients_for_recipe recipe_number recipes_matrix =
  print_endline "Ingredientes: ";
  (* Getting only the ingredients *)
  for i = 3 to 23 do
    let ing_name = recipes_matrix.(recipe_number+1).(i) in
    if ing_name = "" then (
      ();
    ) else (
      let ing_detail = recipes_matrix.(recipe_number+1).(i+1) in 
      print_string (" " ^ ing_detail ^ " ");
      print_string (ing_name ^ ", ");
    )
  done;;

let print_recipe_title recipe_number recipes_matrix =
  print_endline ("Nome: " ^ recipes_matrix.(recipe_number).(1));;

let print_recipe_preparation recipe_number recipes_matrix = 
  print_newline ();
  print_endline "***********************************";
  print_endline "Preparo: ";
  print_string recipes_matrix.(recipe_number+1).(2);
  print_newline ();
  print_endline "***********************************";
  ;;

let rec print_all_recipes_titles recipes_matrix line_number = 
  match line_number with
  | _ when line_number >= Array.length recipes_matrix -> ()
  | _ -> 
    print_int line_number; print_string " - ";
    print_recipe_title line_number recipes_matrix;
    print_endline "******";
    print_all_recipes_titles recipes_matrix (line_number+1);;



let get_all_ingredients recipes_matrix recipe_number =
  let ingredients_list = ref [] in
  let rec get_ingredient i =
  (match i with 
  | _ when i >=  Array.length recipes_matrix.(recipe_number) - 1 -> ();
  | _ -> 
  let ingredient_name = recipes_matrix.(recipe_number).(i) in
  let ing_detail_str = recipes_matrix.(recipe_number).(i+1) in
    (match ing_detail_str with
    | "" -> ();
    | _ ->  
      if ing_detail_str = "" then (();)
      else (
        let is_int = Str.string_match (Str.regexp "[0-9]+$") ing_detail_str 0 in
        let is_float_with_dot = Str.string_match (Str.regexp "[0-9].[0-9]+$") ing_detail_str 0 in
        let is_float_with_comma = Str.string_match (Str.regexp "[0-9],[0-9]+$") ing_detail_str 0 in
        if (is_int  || is_float_with_dot || is_float_with_comma) then (
          let ingredient = {
            name = ingredient_name;
            total = float_of_string ing_detail_str;
            unit = "unidades";
          } in 
          ingredients_list := ingredient :: !ingredients_list;
          get_ingredient (i+2)
        ) 
        else (
          let list_result = split_on_first_space ing_detail_str in
          if List.length list_result = 0 then ()
          else (
            let first = List.nth list_result 0 in 
            let rest = List.nth list_result 1 in
           
            let quantity_number_string = first in
            let is_number_first = Str.string_match (Str.regexp "[0-9]+$") quantity_number_string 0 in
            let is_float_with_dot_first = Str.string_match (Str.regexp "[0-9].[0-9]+$") quantity_number_string 0 in 
            let is_float_with_comma_first = Str.string_match (Str.regexp "[0-9],[0-9]+$") quantity_number_string 0 in
            if (is_number_first || is_float_with_dot_first || is_float_with_comma_first) then (
              let quantity_unit = rest in
              if is_float_with_comma_first then (
                match split_on_first_comma first with
                | [] -> ()
                | [first; rest] ->
                  let new_float_string = first ^ "." ^ rest in 
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
      List.rev !ingredients_list;;


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
       let ingredient = {
          name = final_ings_list.name;
          total = final_ings_list.total;
          unit = final_ings_list.unit;
        } in 
      final_ings_qty_updated := ingredient :: !final_ings_qty_updated;
    )
  done;
  List.rev !final_ings_qty_updated;;


let print_ingredients_summary_for_recipes recipes_matrix recipe1 recipe2 recipe3 = 
  (* Recipes start at line 1 *)
  let recipe_number = recipe1 + 1 in
  let recipe_number2 = recipe2 + 1 in
  let recipe_number3 = recipe3 + 1 in 

  let all_ingredients = all_ingredients_sum recipes_matrix recipe_number recipe_number2 recipe_number3 in 
  print_endline "Ingredientes para as receitas: ";
  print_recipe_title recipe1 recipes_matrix;
  print_recipe_title recipe2 recipes_matrix;
  print_recipe_title recipe3 recipes_matrix;
  let rec print_ingredient_detail i =
  (match i with
  | _ when i >= (List.length all_ingredients - 1) -> print_endline "*************************"; ();
  | _ ->
    let ing_detail = List.nth all_ingredients i in 
    if i = 0 then print_endline "*************************";
    print_int (i+1);
    print_string " - ";
    print_string ing_detail.name;
    print_string " | Quantidade: ";
    print_float ing_detail.total;
    print_string " | Unidade: ";
    print_string ing_detail.unit;
    print_newline ();

    print_ingredient_detail (i+1);
  );
  in print_ingredient_detail 0;;

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
    List.rev !list_of_equals;;


let choose_option number recipes_matrix =
  match number with
  | 1 -> print_all_recipes_titles recipes_matrix 1;
  | 2 ->
      print_endline "Diga o número da receita."; 
      let input_by_user = read_int() in
        print_recipe_title input_by_user recipes_matrix;
      print_ingredients_for_recipe input_by_user recipes_matrix;
      print_recipe_preparation input_by_user recipes_matrix;

  | 3 -> 
    print_endline "Digite o número da receita 1/3 e pressione enter.";
    let recipe_number_1 = read_int() in     
    print_endline "Digite o número da receita 2/3 receitas e pressione enter."; 
    let recipe_number_2 = read_int() in 
    print_endline "Digite o número da receita 3/3 receitas e pressione enter.";
    let recipe_number_3 = read_int() in
    print_ingredients_summary_for_recipes recipes_matrix recipe_number_1 recipe_number_2 recipe_number_3;;
    

let run_code = 
  let lines_read = read_file file_receitas in
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