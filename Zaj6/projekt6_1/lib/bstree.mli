type 'a tree = Lisc | Wezel of 'a tree * 'a * 'a tree
val insert : 'a -> 'a tree -> 'a tree
val to_list : 'a tree -> 'a list
val from_list : 'a list -> 'a tree
