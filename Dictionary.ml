open Crypto

(*********************************************************************
 ***************************SOURCE CODE*******************************
 *********************************************************************
 *    SOURCE: http://old.nabble.com/%22ocaml_beginners%22%3A%3A---   *
 *    red-black-tree-with-constant-time-iterator-td26404529.html     *
 *    ACCESSED: April 23rd, 2013                                     *
 *    AUTHORED BY: rixed                                             *
 *********************************************************************)     

let balance = function 
        | CRYPTO.Black, z, CRYPTO.Node (CRYPTO.Red, y, CRYPTO.Node (CRYPTO.Red, x, a, b), c), d 
        | CRYPTO.Black, z, CRYPTO.Node (CRYPTO.Red, x, a, CRYPTO.Node (CRYPTO.Red, y, b, c)), d 
        | CRYPTO.Black, x, a, CRYPTO.Node (CRYPTO.Red, z, CRYPTO.Node (CRYPTO.Red, y, b, c), d) 
        | CRYPTO.Black, x, a, CRYPTO.Node (CRYPTO.Red, y, b, CRYPTO.Node (CRYPTO.Red, z, c, d)) -> 
                CRYPTO.Node (CRYPTO.Red, y, CRYPTO.Node (CRYPTO.Black, x, a, b), CRYPTO.Node (CRYPTO.Black, z, c, d)) 
        | a, b, c, d -> CRYPTO.Node (a, b, c, d) 
(* The basis of our insert 
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
let condense (e1: CRYPTO.entry) (e2: CRYPTO.entry) : CRYPTO.entry =
  {CRYPTO.scheme = e1.CRYPTO.scheme; CRYPTO.choices = e1.CRYPTO.choices @ e2.CRYPTO.choices}

(* Inserts a one-choice entry into a dictionary, condensing entries with the
 * same scheme. Adapted from cited code above. *)
let insert (x: CRYPTO.entry) (t: CRYPTO.dict) : CRYPTO.dict = 
   let rec ins =   
     function 
     | CRYPTO.Leaf -> CRYPTO.Node (CRYPTO.Red, x, CRYPTO.Leaf, CRYPTO.Leaf) 
     | CRYPTO.Node (color, ent, r, l) -> 
       if x.CRYPTO.scheme < ent.CRYPTO.scheme then balance (color, ent, ins r, l)
       else if x.CRYPTO.scheme > ent.CRYPTO.scheme then balance (color, ent, r, ins l) 
       else CRYPTO.Node (color, condense x ent, r, l) in 
     match ins t with
     | CRYPTO.Node (_, ent, r, l) -> CRYPTO.Node (CRYPTO.Black, ent, r, l)
     | CRYPTO.Leaf -> raise (Invalid_argument "won't have empty subtree")

(* Returns a list of choices for a scheme. *)
let rec lookup (sch: string) (t: CRYPTO.dict) : CRYPTO.choice list =
  match t with
  | CRYPTO.Leaf -> []
  | CRYPTO.Node (color, ent, r, l) ->
    if sch < ent.CRYPTO.scheme then lookup sch r
    else if sch > ent.CRYPTO.scheme then lookup sch l
    else ent.CRYPTO.choices
    
(* Applies a function to all elements in a red-black tree.  
let rec fold (f: 'a -> 'b) (u: 'b) (t: 'a rbtree) : 'b rbtree =
  match t with
  | CRYPTO.Leaf -> u
  | CRYPTO.Node (color, ent, r, l)-> fold (f ent) u r
*)

     



