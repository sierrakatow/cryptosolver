module DICT =
  struct
    type choice = {word : string; freq : int; pos : string list}
    type entry = {scheme : string;choices : choice list}
    type color = Red | Black 
    type 'a rbtree = Node of color * 'a * 'a rbtree * 'a rbtree | Leaf 
    type dict = entry rbtree
  end
;;
 
