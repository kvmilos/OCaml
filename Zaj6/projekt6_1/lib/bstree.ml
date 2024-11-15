type 'a tree = 
  | Lisc
  | Wezel of 'a tree * 'a * 'a tree

let rec insert x t = 
  match t with
  | Lisc -> Wezel(Lisc, x, Lisc)
  | Wezel(l, y, r) ->
    if x < y then Wezel(insert x l, y, r)
    else Wezel(l, y, insert x r)
    
let to_list t =
  let rec pom drzewo acc =
    match drzewo with
    | Lisc -> acc
    | Wezel (l, x, r) ->
        let acc_r = pom r acc in
        let acc_x = x :: acc_r in
        pom l acc_x
  in
  pom t []

let from_list l = 
  List.fold_left (fun acc x -> insert x acc) Lisc l