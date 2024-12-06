(* Zad 10 *)
let rec suma_list l1 l2 = match l1, l2 with
  | [], [] -> []
  | l, [] | [], l -> l
  | h1 :: t1, h2 :: t2 -> if h1 < h2 
    then h1 :: suma_list t1 l2
    else if h1 > h2
    then h2 :: suma_list l1 t2
    else h1 :: suma_list t1 t2
  ;;

(* Zad 11 *)
let rec codrugi l = match l with
  | [] -> []
  | h1 :: _ :: t -> h1 :: codrugi t
  | h1 -> h1
  ;;

let codrugi_fold l = 
  let pom (a1, a2) x = 
    (not a1, if a1 then x :: a2 else a2) in
  let _, wynik = List.fold_left pom (true, []) l
  in List.rev wynik
;;
(* bez List.rev wynik byłby odwrócony *)

(* 
'let _, wynik = List.fold_left pom (true, []) l
  in List.rev wynik'
to to samo co
'snd (List.fold_left pom (true, []) l)' 
*)

(* Zad 12 *)
(* dodałem to exception bo nie wiedzialem co innego zrobić, a znalazłem taki sposób na zwracanie błędów w internecie. mogłem też zwrócić (0,0) albo coś innego, ale to już kwestia jak zdefiniujemy *)
exception EmptyList;;
let plateau l = match l with
  | [] -> raise EmptyList
  | h :: t -> 
    let (cur_val, cur_len, best_val, best_len) = 
      List.fold_left (fun (cur_val, cur_len, best_val, best_len) x -> 
        if x = cur_val
          then (cur_val, cur_len + 1, best_val, best_len)
        else
          let (best_val, best_len) = 
            if cur_len > best_len
              then (cur_val, cur_len)
            else (best_val, best_len)
          in (x, 1, best_val, best_len)
      ) (h, 1, h, 1) t
    in if best_len >= cur_len then (best_val, best_len) else (cur_val, cur_len);;

(* Zad 13 *)
let rec insert x = function
  | [] -> [x]
  | h :: t as l ->
    if x <= h then x :: l
    else h :: insert x t;;

let rec sort = function
  | [] -> []
  | h :: t -> insert h (sort t);;

(* wersje z fold *)
(* POPRAWIONE *)
let insert_fold x l = 
  let (res, inserted) = 
    List.fold_right (fun y (acc, inserted) ->
      if not inserted && x > y then y :: x :: acc, true
      else y :: acc, inserted) l ([], false)
  in if inserted then res else x :: res;;

let sort_fold l = 
  List.fold_left (fun acc x -> insert x acc) [] l;;
(* sort jest łatwiejszy do zamiany*)

(* Zad 14 *)
let rec aplikuj fl x = match fl with
  | [] -> x
  | h :: t -> h (aplikuj t x);;

let aplikuj_fold fl x = 
  List.fold_right (fun f acc -> f acc) fl x;;

(* Zad 15 *)
let rec aplikuj_listy fl al = match fl, al with
  | [], _ -> []
  | _, [] -> []
  | fh :: ft, ah :: at -> (fh ah) :: aplikuj_listy ft at;;

let aplikuj_listy_fold fl al =
  List.rev(List.fold_left2 (fun acc f a -> (f a) :: acc) [] fl al);;
(* fold_left2 różni się od fold_left tym, że fold_left2 bierze 3 argumenty, a fold_left 2. fold_left2 działa na 2 listach naraz *)