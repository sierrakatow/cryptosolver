module CRYPTO =
  struct
    type cryptogram = string
    type answers = string list
    type choice = {word : string; pos : char list}
    type entry = {scheme : string;choices : choice list}
    type color = Red | Black 
    type 'a rbtree = Node of color * 'a * 'a rbtree * 'a rbtree | Leaf 
    type dict = entry rbtree
  end
;;
 
