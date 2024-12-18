type 'a tree = 
  | Lisc
  | Wezel of 'a tree * 'a * 'a tree
  [@@deriving sexp, compare]

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

(* let%test "insertT" = 
  insert 1 Lisc = Wezel(Lisc, 1, Lisc)

let%test "insertF" = 
  insert 1 Lisc = Wezel(Lisc, 2, Lisc)

let%test "to_listT" =
  to_list (Wezel(Wezel(Lisc, 1, Lisc), 2, Wezel(Lisc, 3, Lisc))) = [1; 2; 3]

let%test "to_listF" =
  to_list (Wezel(Wezel(Lisc, 1, Lisc), 2, Wezel(Lisc, 3, Lisc))) = [2; 1; 3]

let%test "from_listT" =
  from_list [1; 2; 3] = Wezel(Lisc, 1, Wezel(Lisc, 2, Wezel(Lisc, 3, Lisc)))

let%test "from_listF" =
  from_list [1; 2; 3] = Wezel(Wezel(Lisc, 1, Lisc), 2, Wezel(Lisc, 3, Lisc)) *)

(* test_eq nie chciało mi działac :/ *)