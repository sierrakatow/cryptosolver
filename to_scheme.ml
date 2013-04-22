let explode cw =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (cw.[i] :: l) in
  expl (String.length cw - 1) [];;

let implode crword =
  let res = String.create (List.length crword) in
  let rec imp i = function
  | [] -> res
  | c :: crword -> res.[i] <- c; imp (i + 1) crword in
  imp 0 crword;;


type word_schm = (char*int) list in
type counter = 0 in
type crword = explode cword in

let rec checkchar (nchar: char) (tuple_list: (char*int) list) : bool =
	match tuple_list with
		[] -> false
		| (c, i)::tl -> if c = nchar then true else checkchar nchar tl
;;


let rec extract (word_schm: char*int list) : int list =
	match word_schm with
	[] -> []
	| (_,i)::tl -> i::(extract tl)
;;


let rec to_scheme crword : string =
	match crword with
	| hd::tl -> if checkchar hd word_schm = false 
		then counter = counter+1 in word_schm@(hd, counter)
		else word_schm@(List.find (fun (a,b) -> hd = a)) in
	implode (extract (List.map (Char.chr (word_schm))))
;;
