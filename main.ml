let main cryptogram = 
  (* load in dictionary *)
  let dict_lst = Load.read_file pos.txt in
  let dict_tree = 
    List.map (fun x y -> Dictionary.insert x y) [] dict_lst in
  let cwords = Str.split (Str.regexp "[ \t]+") cryptogram in
  let choices = 
    List.map (fun s -> Dictionary.lookup (To_scheme.to_schme s)) cwords in
  Decide.decide choices cwords
;;
