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


let decide (lst:choice list list) : choice list =
  let shortest : list = 
    let f l1 l2 = if List.length l1 > List.length l2 then l2 else l1 in
    List.fold_right f lst in
  let keys: (char * char) list list =
    List.map (fun x -> to_key x.word) shortest in


List.mapi ()
