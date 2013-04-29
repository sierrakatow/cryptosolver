#load "str.cma"
open Crypto

(* Takes in a cryptogram and returns answers. *)
let main (c: CRYPTO.cryptogram) : CRYPTO.answers = 
  
  (* Loads dictionary into tree *)
  let dict_lst = Load.read_file pos.txt in
  let dict_tree = 
    List.map (fun x y -> Dictionary.insert x y) Leaf dict_lst in

  (* Breaks cyptogram into a list of cwords *)
  let cwords = Str.split (Str.regexp "[ \t]+") c in

  (* Retrieves possible words from the dictionary to match with cwords *)
  let choices = 
    List.map (fun s -> Dictionary.lookup (To_scheme.to_scheme s)) cwords in

  (* Condenses the best choices into an answer list. No NLP rankings used. *)
  let print_answer (choices : CRYPTO.choice list) = 
    List.fold_right (fun x -> x.word ^ " " ^ y.word) lst ""
  List.map print_answer (Decide.decide choices cwords)
;;
