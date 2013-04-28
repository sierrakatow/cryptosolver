let main crtyptogram = 
  let dict = Load.read_file pos.txt in
  let cwords = Str.split (Str.regexp "[ \t]+") cryptogram in
  let choices = 
    List.map (fun s -> Dictionary.lookup (To_scheme.to_schme s)) cwords in
  Decide.decide choices cwords
;;
