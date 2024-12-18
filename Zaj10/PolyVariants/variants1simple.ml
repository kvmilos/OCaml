(* Polimorficzne warianty - część łatwa *)

(** Wprowadzenie *)

(* Rozpatrzmy taką funkcję: max with payload *)
(* "podaj indeksy fragmentu tablicy, który ma największe cośtam" *)
let maxp px py = 
  if fst px >= fst py then px else py
;;

(* co zrobić jak obie wartości są takie same?*)
let maxp (eq : [`Left | `Right]) px py = 
  let c = compare (fst px) (fst py) in 
  if c < 0 then py 
  else if c > 0 then px
  else if eq = `Left then px else py
;;

(* jeszcze lepsza wersja z domyślną wartością lewą *)
let maxp ?(eq : [`Left | `Right] = `Left) px py = 
  let c = compare (fst px) (fst py) in 
  if c < 0 then py 
  else if c > 0 then px
  else if eq = `Left then px else py
;;

(* to samo, tylko osobno typ w sygnaturze, a osobno (brak typu :) w implementacji *)
module M : sig
  val maxp : ?eq : [`Left | `Right] -> 'a * 'b -> 'a * 'b -> 'a * 'b
end = struct

  let maxp ?(eq = `Left) px py = 
    let c = compare (fst px) (fst py) in 
    if c < 0 then py 
    else if c > 0 then px
    else if eq = `Left then px else py
end;;

(* Więcej podobnych *)
