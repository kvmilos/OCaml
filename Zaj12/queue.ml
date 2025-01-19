class virtual queue = object
  val mutable sum = 0
  val mutable size = 0

  method virtual insert : int -> unit
  method virtual get_min : int
  method get_sum : int = sum
  method get_size : int = size
  method virtual remove_min : unit
end

class stack_queue = object
  inherit queue

  val mutable stack = []

  method insert x =
    let rec insert_sorted x st acc =
      match st with
      | [] -> List.rev (x :: acc)
      | xh :: xt ->
          if x < xh then List.rev_append acc (x :: xh :: xt)
          else insert_sorted x xt (xh :: acc)
    in
    stack <- insert_sorted x stack [];
    sum <- sum + x;
    size <- size + 1

  method get_min = if size > 0 then List.hd stack else failwith "Blad: probujesz pobrac minimum z pustego stosu"
  
  method remove_min =
    match stack with
    | [] -> failwith "Blad: probujesz usunac z pustego stosu"
    | _ ->
        let min = List.hd stack in
        stack <- List.tl stack;
        sum <- sum - min;
        size <- size - 1
end

type bst_tree =
  | Empty
  | Node of { value : int; left : bst_tree; right : bst_tree}

class bst_queue = object 
inherit queue
  val mutable tree = Empty

  method insert x = 
    let rec insert_node t = match t with
      | Empty -> Node { value = x; left = Empty; right = Empty }
      | Node { value = v; left = l; right = r } ->
          if x < v then Node { value = x; left = l; right = t }
          else Node { value = v; left = l; right = insert_node r }
    in
    tree <- insert_node tree;
    sum <- sum + x;
    size <- size + 1
  
  method get_min = match tree with
      | Empty -> failwith "Blad: probujesz pobrac minimum z pustego drzewa"
      | Node { value; _ } -> value

  method remove_min = match tree with 
  | Empty -> failwith "Blad: probujesz usunac z pustego drzewa"
  | Node { value; left; right } ->
      tree <- right;
      sum <- sum - value;
      size <- size - 1
end

let sort (q : #queue) (lst : int list) : int list =
  List.iter q#insert lst;
  let rec collect_sorted acc =
    if q#get_size = 0 then acc
    else
      let min = q#get_min in
      q#remove_min;
      collect_sorted (min :: acc)
  in
  List.rev (collect_sorted [])

let () =
  let sq = new stack_queue in
  sq#insert 5;
  Printf.printf "---[Stos Queue]---Dodaję element 5---\n";
  Printf.printf "[Stos Queue] Minimum: %d\n" sq#get_min;
  Printf.printf "[Stos Queue] Rozmiar: %d\n" sq#get_size;
  Printf.printf "[Stos Queue] Suma: %d\n" sq#get_sum;
  sq#insert 3;
  Printf.printf "---[Stos Queue]---Dodaję element 3---\n";
  Printf.printf "[Stos Queue] Minimum: %d\n" sq#get_min;
  Printf.printf "[Stos Queue] Rozmiar: %d\n" sq#get_size;
  Printf.printf "[Stos Queue] Suma: %d\n" sq#get_sum;
  sq#insert 8;
  Printf.printf "---[Stos Queue]---Dodaję element 8---\n";
  Printf.printf "[Stos Queue] Minimum: %d\n" sq#get_min;
  Printf.printf "[Stos Queue] Rozmiar: %d\n" sq#get_size;
  Printf.printf "[Stos Queue] Suma: %d\n" sq#get_sum;
  sq#remove_min;
  Printf.printf "---[Stos Queue]---Usuwam minimum---\n";
  Printf.printf "[Stos Queue] Minimum: %d\n" sq#get_min;
  Printf.printf "[Stos Queue] Rozmiar: %d\n" sq#get_size;
  Printf.printf "[Stos Queue] Suma: %d\n" sq#get_sum;
  sq#remove_min;
  Printf.printf "---[Stos Queue]---Usuwam minimum---\n";
  Printf.printf "[Stos Queue] Minimum: %d\n" sq#get_min;
  Printf.printf "[Stos Queue] Rozmiar: %d\n" sq#get_size;
  Printf.printf "[Stos Queue] Suma: %d\n" sq#get_sum;
  sq#remove_min;
  Printf.printf "---[Stos Queue]---Usuwam minimum---\n";
  (* Printf.printf "[Stos Queue] Minimum: %d\n" sq#get_min; *)
  Printf.printf "[Stos Queue] Rozmiar: %d\n" sq#get_size;
  Printf.printf "[Stos Queue] Suma: %d\n" sq#get_sum;

  let sorted = sort sq [10; 5; 7; 2; 8] in
  Printf.printf "[Stos Queue] Posortowane: %s\n" (String.concat ", " (List.map string_of_int sorted));

  let bq = new bst_queue in
  bq#insert 5;
  Printf.printf "---[BST Queue]---Dodaję element 5---\n";
  Printf.printf "[BST Queue] Minimum: %d\n" bq#get_min;
  Printf.printf "[BST Queue] Rozmiar: %d\n" bq#get_size;
  Printf.printf "[BST Queue] Suma: %d\n" bq#get_sum;
  bq#insert 3;
  Printf.printf "---[BST Queue]---Dodaję element 3---\n";
  Printf.printf "[BST Queue] Minimum: %d\n" bq#get_min;
  Printf.printf "[BST Queue] Rozmiar: %d\n" bq#get_size;
  Printf.printf "[BST Queue] Suma: %d\n" bq#get_sum;
  bq#insert 8;
  Printf.printf "---[BST Queue]---Dodaję element 8---\n";
  Printf.printf "[BST Queue] Minimum: %d\n" bq#get_min;
  Printf.printf "[BST Queue] Rozmiar: %d\n" bq#get_size;
  Printf.printf "[BST Queue] Suma: %d\n" bq#get_sum;
  bq#remove_min;
  Printf.printf "---[BST Queue]---Usuwam minimum---\n";
  Printf.printf "[BST Queue] Minimum: %d\n" bq#get_min;
  Printf.printf "[BST Queue] Rozmiar: %d\n" bq#get_size;
  Printf.printf "[BST Queue] Suma: %d\n" bq#get_sum;
  bq#remove_min;
  Printf.printf "---[BST Queue]---Usuwam minimum---\n";
  Printf.printf "[BST Queue] Minimum: %d\n" bq#get_min;
  Printf.printf "[BST Queue] Rozmiar: %d\n" bq#get_size;
  Printf.printf "[BST Queue] Suma: %d\n" bq#get_sum;
  bq#remove_min;
  Printf.printf "---[BST Queue]---Usuwam minimum---\n";
  (* Printf.printf "[BST Queue] Minimum: %d\n" bq#get_min; *)
  Printf.printf "[BST Queue] Rozmiar: %d\n" bq#get_size;
  Printf.printf "[BST Queue] Suma: %d\n" bq#get_sum;

  let sorted = sort bq [10; 5; 7; 2; 8] in
  Printf.printf "[BST Queue] Posortowane: %s\n" (String.concat ", " (List.map string_of_int sorted));
  ()
