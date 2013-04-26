(*********************************************************************
 ***************************SOURCE CODE*******************************
 *********************************************************************
 *    SOURCE: http://old.nabble.com/%22ocaml_beginners%22%3A%3A---   *
 *    red-black-tree-with-constant-time-iterator-td26404529.html     *
 *    ACCESSED: April 23rd, 2013                                     *
 *    AUTHORED BY: rixed                                             *
 *********************************************************************)     

(* TYPES *)
type color = Red | Black 
type 'a rbtree = Node of color * 'a * 'a rbtree * 'a rbtree | Leaf 
 
(* ADDITIONAL TYPES 
 * The below is not from the source and is original code. *)
type choice = {word : string; freq : int; pos : string}
type entry = {scheme : string;choices : choice list}
type dict = entry rbtree
(* Cited code resumes*)

let balance = function 
        | Black, z, Node (Red, y, Node (Red, x, a, b), c), d 
        | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d 
        | Black, x, a, Node (Red, z, Node (Red, y, b, c), d) 
        | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) -> 
                Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d)) 
        | a, b, c, d -> Node (a, b, c, d) 
(* THE BASIS FOR OUR INSERT  
let insert x s = 
        let rec ins = function 
                | Leaf -> Node (Red, x, Leaf, Leaf) 
                | Node (color, y, a, b) as s -> 
                        if x < y then balance (color, y, ins a, b) 
                        else if x > y then balance (color, y, a, ins b) 
                        else s in 
        match ins s with (* guaranteed to be non-empty *) 
        | Node (_, y, a, b) -> Node (Black, y, a, b) 
        | Leaf -> raise (Invalid_argument "insert")
*)

(************************** END OF SOURCED CODE ***************************)





(******************************ORIGINAL CODE*******************************)

(* Condenses entries with the same schemes into one entry, appending the 
 * first one-choice entry's choice onto that second entry's choice list. *)
let condense (e1: entry) (e2: entry) : entry =
  {scheme = e1.scheme; choices = e1.choices @ e2.choices}

(* Inserts a one-choice entry into a dictionary, condensing entries with the
 * same scheme. Adapted from cited code above. *)
let insert (x: entry) (t: dict) : dict = 
   let rec ins =   
     function 
     | Leaf -> Node (Red, x, Leaf, Leaf) 
     | Node (color, ent, r, l) -> 
       if x.scheme < ent.scheme then balance (color, ent, ins r, l)
       else if x.scheme > ent.scheme then balance (color, ent, r, ins l) 
       else Node (color, condense x ent, r, l) in 
     match ins t with
     | Node (_, ent, r, l) -> Node (Black, ent, r, l)
     | Leaf -> raise (Invalid_argument "won't have empty subtree")

(* Returns a list of choices for a scheme. *)
let rec lookup (sch: string) (t: dict) : choice list =
  match t with
  | Leaf -> []
  | Node (color, ent, r, l) ->
    if sch < ent.scheme then lookup sch r
    else if sch > ent.scheme then lookup sch l
    else ent.choices


     



