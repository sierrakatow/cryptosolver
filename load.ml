open Crypto

(* Helper function to deal with each line of the dictionary text file *)
let separate (s:string) =
  (* get location where the word ends and the part of speech begins *)
  let separate_location = String.rindex s '\t' in
  let wordstring = String.sub s 0 separate_location in
  let posstring = String.sub s separate_location ((String.length s) - separate_location) in
  (*
   * Separates the pos into a list of chars - useful for words
   * that serve as multiple parts of speech 
   *)
  let rec parse_pos (s:string) : char list = 
    let current_char = String.get s 0 in
    if current_char = '\t' || current_char = '|'
    then parse_pos (String.sub s 1 ((String.length s) - 1))
    else (
      if String.length s = 1
      then (String.get s 0)::[]
      else 
	(String.get s 0)::(parse_pos (String.sub s 1 ((String.length s) - 1))) 
    ) in
  {word = wordstring; pos = parse_pos posstring}
;;

(* read_file parses through text file line by line *)
let read_file filename =
  (* establish output pointer *)
  let lines = ref [] in
  let chan = open_in filename in
  try 
    (* parse until it reaches end of the text file *)
    while true; do
      (*
       * Update the output pointer by appending the added word's
       * record to the existing output list. 
       *) 
      lines := separate (input_line chan) :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    (* Reverse output list to reestablish alphabetical order *)
    List.rev !lines ;;

(* Run function on dictionary text file *)
read_file "pos.txt";;
