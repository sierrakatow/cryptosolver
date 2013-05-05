open Crypto

(* Takes in a cryptogram and returns answers. *)
let main () = 
  
  (* Loads dictionary into red black tree:
     Text file source: wordlist.sourceforge.net *)
  let choice_lst = Load.read_file "Dictionary.txt" in 
  let entry_lst = List.rev_map 
    (fun x -> {CRYPTO.scheme = To_scheme.to_scheme x.CRYPTO.word []; 
	       CRYPTO.choices = [x]}) choice_lst in
  let dict_tree =
    List.fold_left (fun x y -> Dictionary.insert y x) CRYPTO.Leaf entry_lst in
  
  (* Gets input from user. *)
  let c = read_line () in 
  
  (* Breaks cyptogram into a list of cwords. *)
  let cwords = Str.split (Str.regexp "[ \t]+") c in

  (* Retrieves possible words from the dictionary to match with cwords. *)
  let choices = 
    List.map (fun s -> Dictionary.lookup (To_scheme.to_scheme s []) dict_tree) 
      cwords in

  (* Condenses the best choices into an answer list. No NLP rankings used. *)
  let print_answer (choices : CRYPTO.choice list) = 
    List.fold_right (fun x y -> x.CRYPTO.word ^ " " ^ y) choices "\n" in
  
  (* Opens txt file to write answers to. *)
  let answer_file = open_out "answer.txt" in

  (* Retrieve answers. *)
  let answers = List.map print_answer (Decide.decide choices cwords) in
  
  (* Print to file, if answers exist. *)
  match answers with
  | [] -> print_string 
          "No answers could be found with this dictionary. Check for typos!/n"
  (* Prints to "answer.txt" file, then close file. *)
  | _ -> (List.iter (Printf.fprintf answer_file "%s") answers;
    close_out answer_file)
    
(* Runs solver. *)
let _ = main ()
