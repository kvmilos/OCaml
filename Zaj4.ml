(* Zad 1 *)
let rec pierwszy_wiekszy x l = match l with
  | [] -> None
  | h :: t -> 
    if x < h then Some h
    else pierwszy_wiekszy x t

(* Zad 2 *)
let plateau l = match l with 
  | [] -> None
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
    in if best_len >= cur_len then Some (best_val, best_len) else Some (cur_val, cur_len);;

(* Zad 3 *)
type 'a sekwencja = Pusta | Niepusta of 'a * 'a sekwencja;;
let rec m f seq = match seq with
  | Pusta -> Pusta
  | Niepusta (h, t) -> Niepusta (f h, m f t);;


(* Drzewa do testowania *)
type 'a drzewo = 
| Wezel of 'a drzewo * 'a * 'a drzewo
| Lisc;;
let drzewo1 = Lisc;;
let drzewo2 = Wezel (Lisc, 1, Lisc);;
let drzewo3 = Wezel (Wezel (Lisc, 1, Lisc), 2, Wezel (Lisc, 3, Lisc));;
let drzewo4 = Wezel (Wezel (Lisc, 5, Lisc), 4, drzewo3);;
let drzewo5 = Wezel (drzewo4, 7, Wezel (Lisc, 6, Lisc));;

(* Zad 4 *)
let rec wysokosc = function
  | Lisc -> 0
  | Wezel (l, _, r) -> 1 + max (wysokosc l) (wysokosc r);;

wysokosc drzewo1;;
wysokosc drzewo2;;
wysokosc drzewo3;;
wysokosc drzewo4;;
wysokosc drzewo5;;

(* Zad 5 *)
let rec pelne n =
  let rec pom n k = match n with
    | 0 -> Lisc
    | _ -> Wezel (pom (n-1) (k+1), k, pom (n-1) (k+1))
  in pom n 0;;

pelne 0;;
pelne 1;;
pelne 2;;
pelne 3;;

(* Zad 6 *)
let sczytaj drzewo =
  let rec sczytaj_do d acc =
    match d with
    | Lisc -> acc
    | Wezel (l, x, r) ->
        let acc_r = sczytaj_do r acc in
        let acc_x = x :: acc_r in
        sczytaj_do l acc_x
  in
  sczytaj_do drzewo []
;;

sczytaj drzewo1;;
sczytaj drzewo2;;
sczytaj drzewo3;;
sczytaj drzewo4;;
sczytaj drzewo5;;

(* Zad 6, ale odwrotnie *)
(* let sczytaj drzewo =
  let rec sczytaj_do d acc =
    match d with
    | Lisc -> acc
    | Wezel (l, x, r) ->
        let acc_l = sczytaj_do l acc in
        let acc_x = x :: acc_l in
        sczytaj_do r acc_x
  in
  sczytaj_do drzewo []
;; *)

(* Zad 7 *)
let srednica drzewo = 
  let rec wys_sred d = match d with
    | Lisc -> (-1, 0)
    | Wezel (l, _, r) -> 
        let (w_l, s_l) = wys_sred l in
        let (w_r, s_r) = wys_sred r in
        let w = 1 + max w_l w_r in
        let s_k = w_l + w_r + 2 in
        let s = max s_k (max s_l s_r) in
        (w, s) in
  let (_, s) = wys_sred drzewo in
  s;;

srednica drzewo1;;
srednica drzewo2;;
srednica drzewo3;;
srednica drzewo4;;
srednica drzewo5;;

(* Zad 8 *)
let ultralew drzewo = 
  let rec check d current_depth last_depth_opt = match d with
    | Lisc -> ( (* liść *)
      match last_depth_opt with
        | None -> Some current_depth (* pierwszy liść *)
        | Some last_depth -> (* kolejny liść *)
            if current_depth <= last_depth then Some current_depth (* jak narazie ultralew *)
            else None (* nie ultralew *)
      )
    | Wezel (l, _, r) -> (*  *)
      match check l (current_depth + 1) last_depth_opt with
        | None -> None (* nie ultralew w lewym poddrzewie *)
        | Some new_last_depth -> check r (current_depth + 1) (Some new_last_depth)
  in
  match check drzewo 0 None with
    | None -> false
    | Some _ -> true;;

ultralew drzewo1;;
ultralew drzewo2;;
ultralew drzewo3;;
ultralew drzewo4;;
ultralew drzewo5;;

(* Zad 9 *)
let odchudzanie drzewo =
  let rec pom d = match d with
    | Lisc -> (Lisc, 0)
    | Wezel (l, x, p) ->
      let (l_odch, suma_l) = pom l in
      let (p_odch, suma_p) = pom p in
      let suma = suma_l + x + suma_p in
      if suma > 0 then (Wezel (l_odch, x, p_odch), suma)
      else (Lisc, 0) (* odchudzamy jeśli suma <= 0 *)
  in
  fst (pom drzewo);;

odchudzanie drzewo1;;
odchudzanie drzewo2;;
odchudzanie drzewo3;;
odchudzanie drzewo4;;
odchudzanie drzewo5;;

let drzewo6 = Wezel (Wezel (Lisc, -1, Lisc), 0, Wezel (Lisc, 1, Lisc));;
odchudzanie drzewo6;;

let drzewo7 = Wezel (Wezel (Wezel (Lisc, -5, Wezel (Lisc, 2, Lisc)), 3, Wezel (Wezel (Lisc, -8, Lisc), 6, Wezel (Lisc, -1, Wezel (Lisc, 1, Lisc)))), 10, Wezel (Wezel (Lisc, 4, Wezel (Lisc, -2, Lisc)), -7, Wezel (Wezel (Lisc, 7, Wezel (Lisc, -3, Lisc)), 5, Wezel (Wezel (Lisc, 9, Lisc), -10, Wezel (Lisc, -6, Lisc)))));;
odchudzanie drzewo7;;

(* Zad 10 *)
let rec potnij pred drzewo = match drzewo with
  | Lisc -> []
  | Wezel (l, x, p) ->
    if pred x then 
      [Wezel (l, x, p)]
    else 
      let left_cut = potnij pred l in
      let right_cut = potnij pred p in
      left_cut @ right_cut;;
 (* nie umiałem zrobić tego bez używania @, nie wiem czy jest to możliwe *)

let drzewo8 = 
  Wezel (
    Wezel (Wezel (Lisc, 3, Lisc), 10, Wezel (Lisc, 6, Lisc)),
    5,
    Wezel (Lisc, 8, Wezel (Lisc, 9, Lisc))
  );;

potnij (fun x -> x > 2) drzewo1;;
potnij (fun x -> x > 2) drzewo2;;
potnij (fun x -> x > 2) drzewo3;;
potnij (fun x -> x > 2) drzewo4;;
potnij (fun x -> x > 2) drzewo5;;
potnij (fun x -> x > 2) drzewo6;;
potnij (fun x -> x > 2) drzewo7;;
potnij (fun x -> x > 2) drzewo8;;
potnij (fun x -> x > 5) drzewo8;;
