(* KEYS: schemes, VALUES: choice lists *)
 

(* TYPES
type choice = {word;freq;pos}
type entry = {scheme;choice list}
*)

(* NEEDS 
    - to_scheme
*)


(* insert: first time insert scheme. when you insert and something's equal, you
 * take the choice list and add it. *)


(* SOURCE OF ALL BELOW
 * http://old.nabble.com/%22ocaml_beginners%22%3A%3A---
 * red-black-tree-with-constant-time-iterator-td26404529.html *)
type color = Red | Black 
type 'a rbtree = Node of color * 'a * 'a rbtree * 'a rbtree | Leaf 

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

(* THE ABOVE IS REPRODUCED FROM ONLINE (see above for citation) *)

(* Inserting into a rbtree where each value is a list and equal values are
 * consed onto the list. Adapted from cited code above.  *)
let insert (x: 'a) (t: ('a list) rbtree) : ('a list) rbtree  = 
   let rec ins =   
     function 
     | Leaf -> Node (Red, [x], Leaf, Leaf) 
     | Node (color, y, a, b) -> 
       if x < (List.hd y) then balance (color, y, ins a, b)
       else if x > (List.hd y) then balance (color, y, a, ins b) 
       else balance (color, x::y, a, b) in 
     match ins t with
     | Node (_, y, a, b) -> Node (Black, y, a, b)
     | Leaf -> raise (Invalid_argument "won't have empty subtree")


