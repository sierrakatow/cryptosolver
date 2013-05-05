open Crypto

(* creates an initial key from a single word and it's corresponding cword *)
let to_key (wrd:string) (cpt:string) : (char * char) list = 
  let wrd = To_scheme.explode wrd in
  let cpt = To_scheme.explode cpt in
  List.combine wrd cpt
;;

(* Returns true if word character either corresponds to the crypt character in 
 * the key or isn't in the key. *)
let check_char (w_char: char) (c_char: char) (key: (char * char) list)  =  
  (not (List.mem_assoc w_char key) || (List.assoc w_char key) = c_char)
;;
(* "Check, with Unfinished Key"
 * Returns true if a word and a cword are compatible with the key so far.
 * If a letter in the word isn't in the key then it's a match. *)
let rec check_unf_key (word: char list) (cword : char list) 
    (key: (char * char) list) : bool = 
  match word, cword with
  | [], [] -> true
  | whd::wtl, chd::ctl -> (check_char whd chd key && check_unf_key wtl ctl key)
  | _ -> raise(Failure "won't occur, same lengths")
;;

(* updates a key to incorporate information from an additional word that 
 * fits the old key *)
let update_key key wrd cpt = 
  let f w c rest = if not (List.mem_assoc w key) then (w,c)::rest else rest in 
  List.fold_right2 f wrd cpt key
;;
(* sorts a 'a list list by ascending list length *) 
let short_first lst = 
    List.sort (fun x y -> compare (List.length x) (List.length y)) lst 
;;
(* takes the choice list list and returns a list of lists of record containing
 * words and their ordinal position in the answer -- there is probably a more 
 * efficient way to do this *)
type with_info = {w : string; c : string; p : int}
;;
let add_info lst cpt : with_info list list=
  let count = ref 0 in
  let f y x = {w = x.CRYPTO.word; c = y; p = !count} in 
  List.map2 (fun xs y -> count := !count + 1; List.rev_map (f y) xs) lst cpt 
;;

type potential = {key : (char * char) list; ans : with_info list}

(* takes a list of partial keys and answers and updates them with info from a 
   list of choices for a crypto word *)
let cont_key (keys: potential list) (lst: with_info list) : potential list =
  let f k a word rest = 
    let wrd = To_scheme.explode word.w in
    let cpt = To_scheme.explode word.c in
    if check_unf_key wrd cpt k then 
      {key = update_key k wrd cpt; ans = word :: a} :: rest
    else rest in
  List.flatten (List.rev_map (fun x -> List.fold_right (f x.key x.ans) lst []) keys)
;;
(* Takes the original cryptogram (in list form) and a list containing lists of all
 * possible choices for each word in the crytogram. Returns a list of all 
 * possible answers where every word conforms to a single key.
 * "to_choices" is a helper function that puts the with_info list of words back 
 * into the order in which they appeared in the cryptogram and turns them back 
 * into choices*)
let decide (choices:CRYPTO.choice list list) (cpt:string list) : 
    CRYPTO.choice list list =
  let lst = short_first (add_info choices cpt) in
  let start_keys =
    List.rev_map (fun x -> {key = (to_key x.w x.c); ans = [x]}) (List.hd lst) in
  let rec complete_key keys remaining =
    if remaining = [] then keys
    else match List.tl remaining with
         | []-> keys
         | hd::tl -> complete_key (cont_key keys hd) tl in
  let to_choices (a: potential): CRYPTO.choice list = 
    let ordered = List.sort (fun a b -> compare a.p b.p) a.ans in 
    (* DEBUGGING *) let _ = List.map (fun x -> print_string ("\n" ^ x.w ^ "\n")) ordered in
    let find_choice (xs:CRYPTO.choice list) (y:with_info) = 
      List.find (fun x -> (*print_string ("\nchoice word:" ^ x.CRYPTO.word ^ "   "); print_string ("match:" ^ y.w ^ "\n"); *) x.CRYPTO.word = y.w) xs in
    List.map2 find_choice choices ordered in
  List.map to_choices (complete_key start_keys lst)
;;
