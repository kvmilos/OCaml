(* Zad 1 *)
let l = [[]; []];;
(* typ l to 'a list list*)

let k = [[]; []; []];;
let m = [[]];;
(* inne wyrażenia tego typu to k i m *)

let l2 = [[[]]];;
(* typ l2 to 'a list list list *)

let l3 = [[]; [[]]];;
(* l3 jest poprawne, bo OCaml podnosi typy, gdy elementy listy muszą mieć wspólny typ, a ich aktualne typy różnią się tylko poziomem zagnieżdżenia. typ l3 to 'a list list list. Rozumiem, że to działa tylko gdy mamy np. puste listy, bo gdyby dodać coś do ich środka, nawet jeśli do obu list dodamy int, to już nie zadziała - ale nie wiem jak to dobrze ubrać w słowa.
let l3_zle = [[[1]]; [1]];;
l3_zle jest niepoprawne *)

(* Zad 2 *)
let drugi_z_pierwszym lista = match lista with
  | (a1 :: a2 :: ta) :: (b1 :: tb) :: t -> a2 + b1
  (* w powyższej linijce może to też być "(_ :: a :: _) :: (b :: _) :: _ -> a + b", zapisalem to w ten sposób, żeby móc spojrzeć w ten plik i mieć tak jakby definicję :) *)
  | _ -> invalid_arg "lista jest nieodpowiednia";;

(* Zad 3 *)
let rec dlugosc l = match l with
  | [] -> 0
  | a :: t -> 1 + dlugosc t;;

let dlugosc_og l =  
  let rec dlugosc_og_pom l acc = match l with
    | [] -> acc
    | a :: t -> dlugosc_og_pom t (acc+1)
  in
  dlugosc_og_pom l 0;;

let rec rosnace n = 
  let rec pom i =
    if i > n then []
    else i :: pom (i+1)
  in
  pom 1;;

let rosnace_og n = 
  let rec rosnace_og_pom n acc = 
    if n = 0 then acc
    else rosnace_og_pom (n-1) (n :: acc)
  in
  rosnace_og_pom n []

(* Zad 4 *)
let odwroc l =
  let rec pom l acc = match l with
  | [] -> acc
  | a :: t -> pom t (a :: acc)
  in 
  pom l [];;

(* Zad 5 *)
let rec bez_ost l = match l with
  | [] -> []
  | [_] -> []
  | h :: t -> h :: bez_ost t;;
(* złożoność tej funkcji to O(n), z tego co mi się wydaje, to nie da się jej "poprawić" bardziej *)

let rec poczatki l = match l with
  | [] -> [[]]
  | _ -> l :: poczatki (bez_ost l);;
(* ta funkcja nie może działać ze złożonością O(n), w przeciwie do funkcji ogony, tutaj mamy złożoność O(n**2) *)

(* funkcja map *)
let rec map f l = match l with
  | [] -> []
  | h :: t -> f h :: map f t
;;

(* Zad 6 *)
let rec f a b c = match a :: c with
  | _ :: _ :: t -> b :: f a b t
  | _ -> [b];;

(* Zad 7 *)
let odwroc_wszystkie l = 
  map (fun x -> odwroc x) l;;
(* trochę bez sensu w taki sposób to robić, ale na potrzeby zadania tak zapisałem, żeby użyć funkcji anonimowej *)
let odwroc_wszystkie_og l = map odwroc l;;
(* to bardziej intuicyjne rozwiązanie *)

(* Zad 8 *)
let parzyste l = 
  List.filter (fun x -> x mod 2 = 0) l;;

(* Zad 9 *)
let polowki l = 
  List.map (fun x -> x / 2) (List.filter (fun x -> x mod 2 = 0) l);;
