type choice = {word:string; freq:int; pos:char}

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

(* Returns true if word character either corresponds to the crypt character in 
 * the key or isn't in the key. *)
let check_char (w_char: char) (c_char: char) (key: (char * char) list)  =  
  (not (List.mem_assoc w_char key) || (List.assoc w_char key) = c_char)

(* "Check, with Unfinished Key"
 * Returns true if a word and a cword are compatible with the key so far.
 * If a letter in the word isn't in the key then it's a match. *)
let rec check_unf_key (word: char list) (cword : char list) 
    (key: (char * char) list) : bool = 
  match word, cword with
  | [], [] -> true
  | whd::wtl, chd::ctl -> (check_char whd chd key && check_unf_key wtl ctl key)
  | _ -> raise(Failure "won't occur, same lengths")


(* updates a key to incorporate information from an additional word that 
 * fits the old key *)
let update_key key wrd cpt = 
  let f w c rest = if not (List.mem_assoc w key) then (w,c)::rest else rest in 
  List.fold_right2 f wrd cpt key

(* sorts a 'a list list by ascending list length *) 
let short_first lst : list = 
    List.sort (fun x y -> compare (List.length x) (List.length y)) lst in

(* the main decide function *)
let decide (lst:choice list list) : choice list =
  match lst with

(*
let keys: (char * char) list list =
    List.map (fun x -> to_key x.word) shortest in

[{key = , ans = [(word, position)]}]
*)
