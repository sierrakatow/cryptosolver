#load "str.cma"
open Crypto

(* Takes in a cryptogram and returns answers. *)
let main (c: Crypto.cryptogram) : answers = 
  let dict_lst = Load.read_file pos.txt in
  let dict_tree = 
    List.map (fun x y -> Dictionary.insert x y) Leaf dict_lst in
  let cwords = Str.split (Str.regexp "[ \t]+") c in
  let choices = 
    List.map (fun s -> Dictionary.lookup (To_scheme.to_schme s)) cwords in
  List.map (fun x -> ) Decide.decide choices cwords
;;
