open Crypto

let explode cw =
(*explodes a string into a list of chars by iterating over the string and 
 adding each letter to a list*)
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (cw.[i] :: l) in
  expl (String.length cw - 1) [];;

let implode crword =
(*implodes a list of chars into a string*)
  let res = String.create (List.length crword) in
  let rec imp i = function
  | [] -> res
  | c :: crword -> res.[i] <- c; imp (i + 1) crword in
  imp 0 crword;;


type word_schm = (char*int) list 
(* type crword = explode cword *)

let rec checkchar (nchar: char) (tuple_list: (char*int) list) : bool =
(*checks if a char has already appeared in the list of tuples*)
  match tuple_list with
  | [] -> false
  | (c, i)::tl -> (c = nchar || checkchar nchar tl)
;;


let rec extract (word_scheme: word_schm) : int list =
(*extracts the ints from the char*int list*)
	match word_scheme with
	| [] -> []
	| (_,i)::tl -> (i + 64)::(extract tl)
;;

(* Turns words into schemes of their letters, i.e. "YOLO" -> "ABCB" *)
let to_scheme cword key : string = 
  let counter = ref 0 in
  let rec to_scheme_r (cword: string) (key: (char * int) list) : string =
    match (explode cword) with
    | [] -> implode (List.map Char.chr (extract (key)))
    | hd::tl ->
        if checkchar hd key = false
               (*if the character has not been in the word before, increment 
	         the counter and add the tuple of the character and counter to
	         the list of tuples*)
        then ( 
	    counter := !counter + 1; 
	    to_scheme_r (implode tl) (key @ [(hd, !counter)])
	      )
                (*if the character is a repeat, find the number matched with it
		  previously and add the same tuple into the list again*)
        else (
	    to_scheme_r (implode tl) 
              (key @ [(List.find (fun (a,b) -> hd = a) key)]); 
	  )
                (*extract the ints from the list of tuples, then turns them 
		  into their char equivalents and implodes the new list of 
		  chars into a string*)
  in 
  to_scheme_r cword key 
;;
