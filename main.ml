open Crypto

(* Takes in a cryptogram and returns answers. *)
let main (c: CRYPTO.cryptogram) = 
  (* Loads dictionary into tree *)
  let choice_lst = Load.read_file "Dictionary.txt" in 
  let entry_lst = List.rev_map 
    (fun x -> {CRYPTO.scheme = To_scheme.to_scheme x.CRYPTO.word []; 
	       CRYPTO.choices = [x]}) choice_lst in
 let dict_tree =
   List.fold_left (fun x y -> Dictionary.insert y x) CRYPTO.Leaf entry_lst in

  (* Breaks cyptogram into a list of cwords *)
  let cwords = Str.split (Str.regexp "[ \t]+") c in

  (* Retrieves possible words from the dictionary to match with cwords *)
  let choices = 
    List.map (fun s -> Dictionary.lookup (To_scheme.to_scheme s []) dict_tree) 
      cwords in

  (* Condenses the best choices into an answer list. No NLP rankings used. *)
 let print_answer (choices : CRYPTO.choice list) = 
    List.fold_right (fun x y -> x.CRYPTO.word ^ " " ^ y) choices "\n" in
  let answers = List.map print_answer (Decide.decide choices cwords) in
  List.iter print_string answers 


let _ = main Sys.argv.(1) 
