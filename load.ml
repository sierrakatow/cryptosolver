type entry = {word: string; pos: char list};;
let separate (s:string) = 
  let separate_location = String.rindex s '\t' in
  let wordstring = String.sub s 0 separate_location in
  let posstring = String.sub s separate_location ((String.length s) - separate_location) in
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
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try 
    while true; do
      lines := separate (input_line chan) :: !lines
    done; []
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;
