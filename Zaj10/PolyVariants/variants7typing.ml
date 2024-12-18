
(********************************************)
(* Dlaczego to takie skomplikowane? :) *)
(* Większe? Mniejsze? Oba naraz? O co w tym chodzi? *)
(* Tak musi być! :) *)

(* Po pierwsze: silne i słabe zmienne typowe *)

let l = [];;  (* Silna zmienna! Od razu została zgeneralizowana *)

let l1 = 5 :: l;;

let l2 = "a" :: l;;

let f1 = function [] -> 0 | a :: _ -> a
let w1 = f1 l;;

let f2 = function [] -> "nic" | s :: _ -> s;;
let w = f2 l;;
(* Nic jej nierusza! 'a list oznacza tak naprawdę  ∀α. α list *)


(* słabe zmienne *)

let lr = ref [];;  (* to nie jest zgeneralizowane! To jest wolna zmienna typowa *)
                   (* ona może się jeszcze zmienić przez unifikację *)

(* let _ =  lr := ("a" :: !lr);; *)
(* lr;; *)
(* lr : string list ref *)

(* let _ = begin  lr := (5 :: !lr);  List.iter print_int !lr  end;;   *)
(* lr;; *)
(* lr : int list ref *)

(* Nie można zgeneralizować typu ref [], bo nie wiadomo co tak naprawdę będzie pod ref... Pierwsze użycie "wygrywa" *)
(* Ale nie wiadomo kiedy (może gdzieś w środku dużego wyrażenia) tworzy sie ref... *)
(* Więc nie generalizujemy żadnej aplikacji (f x) - bo tam się potencjalnie wykonuje jakiś kod - być może ref *)
(* W pozostałych przypadkach składniowych (funkcja, konstruktor, ...) - generalizujemy *)


(******************************************)
(* Teraz wracamy do polymorphic variants: *)


let show_apple `Apple = "jabłko";;
let show x = match x with `Orange s -> "orange "^s | `Apple -> "apple";;

let r1 = ref `Apple;;
(* r1 : _[> `Apple] ref *)
let w = show !r1 ;;
(* w : string = "apple" *)
r1;;
(* r1 : _[< `Apple | Orange of string > `Apple] ref *)

(***** check-point ****)

r1 := `Orange "Spain";;
r1;;
(* r1 : [`Apple | `Orange of string] *)

(***** check-point again ****)

(* let _ = show_apple !r1;; *)
(* r1;; *)
(* r1 : [`Apple ]  *)




(* Te ref-y powyżej pozwalają pokazać cały proces krok po kroku, ale nie są one zasadnicze.  *)
(* Te same unifikacje są możliwe podczas typowania (trochę większego) wyrażenia *)