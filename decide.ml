type choice = {word:string; freq:int; pos:char};;

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) [];;

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l;;

(* creates an initial key from a single word and it's corresponding cword *)
let to_key (wrd:string) (cpt:string) : (char * char) list = 
  let wrd = explode wrd in
  let cpt = explode cpt in
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
let add_info lst cpt =
  let f i x = {w = x.word; c = ""; p = i} in
  let with_pos = List.mapi (fun i xs -> List.map (f i) xs) lst in
  let f2 y x = {x with c = y} in 
  List.map2 (fun xs c -> List.map (f2 c) xs) with_pos cpt 
;;
type potential = {key : (char * char) list; ans : with_info list}

(* takes a list of partial keys and answers and updates them with info from a 
   list of choices for a crypto word *)
let cont_key (keys: potential list) (lst: with_info list) : potential list =
  let f k a word rest = 
    let wrd = explode word.w in
    let cpt = explode word.c in
    if check_unf_key wrd cpt k then 
      {key = update_key k wrd cpt; ans = word :: a} :: rest
    else rest in
  List.flatten (List.map (fun x -> List.fold_right (f x.key x.ans) lst []) keys)
;;
(* the main decide function:
 * takes the original cryptogram in list form and a list containing lists of all
 * possible choices for each word in the crytogram. returns a list of all 
 * possible answers where every word conforms to a single key *)
let decide (choices:choice list list) (cpt:string list) : choice list list =
  let lst = short_first (add_info choices cpt) in
  let start_keys =
    List.map (fun x -> {key = (to_key x.w x.c); ans = [x]}) (List.hd lst) in
  let rec complete keys =
    match List.tl lst with
    | []-> keys
    | hd::tl -> complete (cont_key keys hd) in
  let to_choices (a: potential): choice list = 
    let ordered = List.sort (fun a b -> compare a.p b.p) a.ans in
    let find_choice (xs:choice list) (y:with_info) = 
      List.find (fun x -> x.word = y.w) xs in
    List.map2 find_choice choices ordered in
  List.map to_choices (complete start_keys)

;;
