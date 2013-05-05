open Crypto

(* Creates an initial key from a single word and it's corresponding cword. *)
let to_key (wrd:string) (cpt:string) : (char * char) list = 
  let wrd = To_scheme.explode wrd in
  let cpt = To_scheme.explode cpt in
  List.combine wrd cpt
;;

(* Returns true if either a character from a word or a character from a 
 * cryptoword is present in a key. *)
let key_mem (w:char) (c:char) (k:(char * char) list) : bool =
  List.exists (fun x -> let (a,b) = x in a = w || b = c) k
;;

(* Returns true if word character either corresponds to the crypt character in 
 * the key or isn't in the key. *)
let check_char (w_char: char) (c_char: char) (key: (char * char) list)  =   
  (not (key_mem w_char c_char key)) ||
     List.exists (fun x -> let (w,c) = x in w = w_char && c = c_char) key
;;

(* TESTING *)
let _ = 
  let k =[('a', 'b'); ('x', 'y'); ('y', 'h')] in
  assert(check_char 'a' 'b' k);
  assert(check_char 'x' 'y' k);
  assert((check_char 'y' 'x' k) = false);
  assert((check_char 'a' 'h' k) = false);
  assert(check_char 'h' 'a' k);
  assert((check_char 'y' 'y' k) = false)
;;


(* "Check, with Unfinished Key"
 * Returns true if a word and a cword are compatible with the key so far.
 * If a letter in the word isn't in the key then it's a match. *)
let rec check_unf_key (word: char list) (cword : char list) 
    (key: (char * char) list) : bool = 
  match word, cword with
  | [], [] -> true
  | whd::wtl, chd::ctl -> (check_char whd chd key && check_unf_key wtl ctl key)
  | _ -> raise (Failure "won't occur, same lengths")
;;

(* updates a key to incorporate information from an additional word that 
 * fits the old key *)
let update_key key wrd cpt = 
  let f w c rest = if not (key_mem w c key) then (w,c)::rest else rest in 
  List.fold_right2 f wrd cpt key
;;
(* sorts a 'a list list by ascending list length *) 
let short_first lst = 
    List.sort (fun x y -> compare (List.length x) (List.length y)) lst 
;;
(* Takes the choice list list and returns a list of lists of record containing
 * words and their ordinal position in the answer *)
type with_info = {w : string; c : string; p : int}
;;

let add_info lst cpt : with_info list list=
  let count = ref 0 in
  let f y x = {w = x.CRYPTO.word; c = y; p = !count} in 
  List.map2 (fun xs y -> count := !count + 1; List.rev_map (f y) xs) lst cpt 
;;

type potential = {key : (char * char) list; ans : with_info list}

(* Takes a list of partial keys and answers and updates them with info from a 
   list of choices for a crypto word *)
let cont_key (keys: potential list) (lst: with_info list) : potential list =
  let f k a word rest = 
    let wrd = To_scheme.explode word.w in
    let cpt = To_scheme.explode word.c in
    if check_unf_key wrd cpt k then 
      {key = update_key k wrd cpt; ans = word :: a} :: rest
    else rest in
  List.flatten (List.rev_map (fun x -> List.fold_right (f x.key x.ans) lst [])
    keys)
;;
(* Takes the original cryptogram (in list form) and a list containing lists of 
 * all possible choices for each word in the crytogram. Returns a list of all 
 * possible answers where every word conforms to a single key.
 * "to_choices" is a helper function that puts the with_info list of words back 
 * into the order in which they appeared in the cryptogram and turns them back 
 * into choices*)
let decide (choices:CRYPTO.choice list list) (cpt:string list) : 
    CRYPTO.choice list list =
    
  (* Sorts list of choices for each word by length, shortest first. *)
  let lst = short_first (add_info choices cpt) in
  
  (* Initializes first keys for each choice. *)
  let start_keys =
    List.rev_map (fun x -> {key = (to_key x.w x.c); ans = [x]}) (List.hd lst) in
    
  (* Completes a key by cross-key optimization. *)
  let rec complete_key keys remaining =
    match remaining with
    | []-> keys
    | hd::tl -> complete_key (cont_key keys hd) tl in
    
  (* Returns a words potential choices. *)
  let to_choices (a: potential): CRYPTO.choice list = 
  
    (* Sorts answers by length. *)
    let ordered = List.sort (fun a b -> compare a.p b.p) a.ans in 
    
    (* Find the choice with a specific word. *)
    let find_choice (xs:CRYPTO.choice list) (y:with_info) = 
      List.find 
       (fun x -> print_string ("\nchoice word:" ^ x.CRYPTO.word ^ "   ");
       print_string ("match:" ^ y.w ^ "\n"); x.CRYPTO.word = y.w) xs in
    
    (* Finds choices for each cword. *)   
    List.map2 find_choice choices ordered in
  
  (* Find choices for each word with cross-choice optimization. *)  
  List.map to_choices (complete_key start_keys (List.tl lst))
;;
