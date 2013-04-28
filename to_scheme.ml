let explode cw =
(*explodes a string into a list of chars by iterating over the string and adding each letter to a list*)
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


type word_schm = (char*int) list in
type counter = 0 in
type crword = explode cword in

let rec checkchar (nchar: char) (tuple_list: (char*int) list) : bool =
(*checks if a char has already appeared in the list of tuples*)
	match tuple_list with
		[] -> false
		| (c, i)::tl -> if c = nchar then true else checkchar nchar tl
;;


let rec extract (word_schm: char*int list) : int list =
(*extracts the ints from the char*int list*)
	match word_schm with
	[] -> []
	| (_,i)::tl -> i::(extract tl)
;;


let rec to_scheme crword : string =
(*turns cryptowords into schemes*)
	match crword with
	| hd::tl -> if checkchar hd word_schm = false 
		then counter = counter+1 in word_schm@(hd, counter)
                (*if the character has not been in the word before, increment the counter and add the tuple of the character and counter to the list of tuples*)
		else word_schm@(List.find (fun (a,b) -> hd = a)) in
                (*if the character is a repeat, find the number matched with it previously and add the same tuple into the list again*)
	implode (List.map (Char.chr (extract (word_schm))))
        (*extracts the ints from the list of tuples, then turns them into their char equivalents and implodes the new list of chars into a string*)
;;
