(** "Normalne" klasy i obiekty w OCamlu - prawie jak w Javie **)

class point = object
  val mutable x = 0   (* oczywiście nie musi być mutable *)
  method get_x = x
  method move d = x <- x + d  (* zmiana, podobna do rekord.x <- wartość, albo tablica.[i] <- wartość *)
end;;

let p = new point;;

p#get_x;;

p#move 10;;

p#get_x;;

(* typy dla obiektów - "object type" - generują się automatycznie i często mają .. "elision" (row polymorphism)*)
let f p = 
  print_int p#get_x; print_newline (); 
  p#move 10;
  print_int p#get_x; print_newline ()
;;
  
f p;;

class grid_print_point x_init =
  let origin = (x_init / 10) * 10 in  (* "inicjalizacja" przed utw. obiektu *)
  object (self)   (* self to zmienna związana - niby możemy ją nazwać jak chcemy *)
    inherit point as super (* super to też zmienna związana - j.w. *)
    val! mutable x = origin  (* że zastępujemy, ale nie możemy zmienić typu, bez ! warning *)
    method print = print_int super#get_x  (* tutaj to wszystko jedno czy self czy super *)
    initializer print_string "new point at "; self#print; print_newline () (* inicj.. po utworzeniu *)
  end;;

let gp = new grid_print_point 75;;

f gp;;

(* [p; gp];; (* p i gp mają różne typy *) *)

[p; (gp :> point) ];;  (* teraz ok *)

(* let l : <get_x : int; .. > list = [p; gp];; *)
(* '..' to zmienna typowa - po p zmienna '..' to "move : int -> unit" i koniec. 
   Typ gp już do tego nie pasuje *)

(* Dopasowanie (typu) funkcji do argumentu odbywa się poprzez polimorfizm ..  *)
(* Dwa obiekty nie chcą się do siebie dopasować, trzeba użyć koercji :> *)

(* typy obiektowe (z ..) to takie nienazwane interfejsy - nie deklaruje ich się w klasach *)

(* inherit zasłania rzeczy (zmienne, metody) zdefiniowane wcześniej
    - przenieśmy val x i dodajmy dziwne get_x ponad inherit *)

class strange_point x_init =
  let origin = (x_init / 10) * 10 in  (* "inicjalizacja" przed utw. obiektu *)
  object (self)   (* self to zmienna związana - niby możemy ją nazwać jak chcemy *)
    val mutable x = origin  (* niby zastępujemy *)
    inherit! point as super (* super to też zmienna związana - j.w. *)
    method get_x = x + 1000
    method print = print_int super#get_x  (* jak get_x po inherit, to self i super działają inaczej *)
    initializer print_string "new point at "; self#print; print_newline () (* inicj.. po utworzeniu *)
  end;;

let sp = new strange_point 75;;
  
f sp;;
  
(** private - metody dostępne tylko w metodach, również w podklasach (Java - protected) **)

class klasa = object
  val z = 5
  method private str = string_of_int z
  method get_z = z
end;;

let k = new klasa;;

k#get_z;;

(* k#str;; *)
(* niepoprawne! *)

class podklasa0 = object (self)
  inherit klasa
  method print = print_endline self#str
end;;

let p0 = new podklasa0;;

p0#print;;

(* p0#str;; *)
(* wciąż nielegalne *)

(** Wyciąganie prywatnego **)
(* 1. przez super *)
class podklasa1 = object (self)
  inherit klasa as super
  method str = super#str
end;;

let p1 = new podklasa1;;

p1#str;;
(* ok *)

(* 2. przez wymuszenie typu self: *)

class podklasa2 = object (self : <str:string; ..> )
  inherit klasa
end;;

let p2 = new podklasa2;;

p2#str;;
(* ok *)


(* 3. przez (nieprywatną :) metodę "wirtualną" *)
class podklasa3 = object (self)
  inherit klasa
  method virtual str : string  (* albo : _ *)
end;;

let p3 = new podklasa3;;

p3#str;;
(* ok *)

(** Metody i klasy virtual (tak normalnie) **)

class virtual abstrakcyjna = object 
  val virtual x : int
  val y = 15
  method virtual print : unit (* wirtualna może być prywatna *)
end;;

(* let a = new abstrakcyjna;; *)
(* nielegalne *)

class virtual niekonkretna = object
  inherit abstrakcyjna
  val x = 10
  method virtual print : _ (* i już nie jest prywatna :) *)
end;;

(* let n = new niekonkretna;; *)
(* wciąż nielegalne *)

class konkretna = object
  inherit niekonkretna
  method print = Printf.printf "< %d, %d >\n" x y
end;;

let k = new konkretna;;
(* teraz ok *)

k#print;;

(* Wsparcie dla obiektów niezmienialnych *)

class punkt = object
  val x = 0
  val y = 0
  method przesun dx dy = {< x = x+dx; y = y + dy >}  (* polimorficzne kopiowanie obiektu (z modyfikacją) *)
  method print = Printf.printf "< %d, %d >\n" x y
end;; 

let p = new punkt;;
p#print;;

let p10 = p#przesun 10 10;;
p10#print;;
p#print;;

let p20 = p10#przesun 10 10;;
p20#print;;

class kolorowy_punkt kol = object(self)
  inherit punkt as super
  val kolor = kol
  method dajk = kolor
  method print = Printf.printf "%s" self#dajk; super#print
  method przesun dx dy = {< x = x+dx; y=y+dy; kolor=kolor^"a" >}
end;;

let pk = new kolorowy_punkt "czerwony";;
pk#print;;

let pk1 = pk#przesun 7 7;;
pk1#print;;


(* Obiekty bez klasy *)

let mk x0 = object val x = x0 method print = Printf.printf "%d\n" x end;;

(* ale nie ma jak dziedziczyć - trochę jak new Object(){...} w Javie *)

k;;
let _ = [k; mk 10];;
(* co to właściwie jest "konkretna" ??? *)
(* #show konkretna;; - uwaga w OCamlu 4.13 robi seqfault :) *)

(* Class types *)
(* Co to? Tak naprawdę to coś co się wypisuje tu i można wpisać w module type *)

module M = struct
  class klasa x0 = object (self)
    val x = x0 
    method private get_x = x
    method print = Printf.printf "%d\n" self#get_x 
  end
end;;

module type MSIG = sig
  class klasa : int -> object method print : unit end
end;;

module M1 : MSIG = M;;

let m1 = new M1.klasa 17;;
m1#print;;

(* to "coś co można wpisać w module type" to właśnie class type i można to nazwać: *)

class type ktyp = object 
  method private get_x : int 
  method print : unit 
end;;

class kl : int -> ktyp = M.klasa;;

(* klasa wirtualna z class type :) *)
class virtual kv = object (self : #ktyp) end;;

(* nie można chować nie-prywatnych metod *)

class type ktyp2 = object method private get_x : int end;;

(* class kl2 : ktyp2 = M.klasa 25;; *)
(* public method cannot be hidden *)

(* Ale właściwie dlaczego? *)
(* Bo klasy definiują również typy i tu wchodzi kontrawariancja! *)
(* https://discuss.ocaml.org/t/why-can-t-you-hide-public-or-virtual-members-in-classes-via-interfaces/1617/6 *)

(* gdyby to było dobre... *)
(* 
module M : sig
  class c : object end
  val f : c -> int
end = struct
  class c = object method foo = 3 end
  let f c = c#foo
end
*)
(* to to też... *)
(* let _ = M.f (object end);; *)

(* metodę można oczywiście schować na poziomie typu obiektu, ale nie klasy *)

(** Wielokrotne dziedziczenie **)
(* https://v2.ocaml.org/releases/5.1/htmlman/objectexamples.html#s%3Amultiple-inheritance *)

(* dziedziczenie "po kolei" - konflikty uniemożliwiają *)

(* konflikt metod to prawdziwy problem *)
class nad1 = object method m = "ala" end;;
class nad2 = object method m = 42 end;;

class pod = object
  inherit nad1
  (* inherit nad2 *)
  (* no way! *)
end

(* konflikt samych atrybutów (object variables) można obejść przez podklasę *)
class nad1 = object val m = "ala" end;;
class nad2 = object val m = 42 end;;

class zm1 = object inherit nad1  method private tajne_get_m = m  end
class type schowane_zm1 = object method private tajne_get_m : string end

class pod = object
  (* inherit nad1 *)
  inherit (zm1 : schowane_zm1) as nad1 
  inherit nad2
  method get_m1 = nad1#tajne_get_m 
  method get_m2 = m
end;; (* tu jeszcze można by schować prywatną metodę tajne_get_m *)



(** Polimorficzne klasy **)


class ['a] oref x_init =
    object
      val mutable x = (x_init : 'a)
      method get = x
      method set y = x <- y
    end;;

(* składnia #klasa - oznacza dowolną podklasę (w sensie podtypu - "co najmniej te metody") *)
(* tak jakby z typu <metoda1; metoda2> zrobić <metoda1; metoda2; .. > *)


(** Polimorficzne metody *)

class ['b] lista (l : 'b list) =
    object
      method empty = (l = [])
      method fold : 'a. ('a -> 'b -> 'a) -> 'a -> 'a =
        fun f accu -> List.fold_left f accu l
    end;;


(** Metody binarne *)


class virtual comparable =
    object (_ : 'a)
      method virtual leq : 'a -> bool
    end;;


    class money (x : float) =
    object
      inherit comparable
      val repr = x
      method value = repr
      method leq p = repr <= p#value
    end;;

let f (c : #comparable) a = c#leq a;;

f (new money 3.5) (new money 4.5);;

(** Friend classes *)

(* trzeba używać modułów i abstrakcji *)

(* przykłady bibliotek używających obiektów:

https://github.com/xavierleroy/cryptokit/blob/master/src/cryptokit.mli
http://cristal.inria.fr/camlimages/

Rozdział 12 i 13 Real World Ocaml jest też o obiektach:
https://dev.realworldocaml.org/objects.html
https://dev.realworldocaml.org/classes.html

*)




(** Słowniczek:
 
(w wartościach)
object ... end - (anonimowy) ostateczny kreator każdego obiektu (nie ma w Javie) 
                 (ale używany poza klasą to taki: new Object(){...} (J))

< ... > - "object type" typ (ocamlowy) dla obiektów (anonimowy interfejs (~J))

class - nazwany dziedziczalny ;) kreator obiektów (klasa (J))

(w typach klas)
object ... end - "obietnica" stworzenia obiektu o zadanych parametrach 
                  Opis co tak naprawdę zrobi klasa - na tej podstawie stwierdzamy czy klasy 
                  do siebie pasują czy nie - myślimy tu o modułach / typach modułów, 
                  ale też class type (patrz niżej)

new - wywołanie klasy i kreatora w niej zawartego (stworzenie obiektu - jak new w Javie)

method private - metody widoczne tylko w klasie i w podklasie (na zewnątrz nie) (protected (J)) 

class / method / val virtual - klasa / metoda / atrybut abstrakcyjny, 
  klasa z wirtualnymi elementami, met/atr - podany tylko typ (atrybut - niezainicjalizowany...)

class type - opis co stworzy klasa (być może z pewnym zatajeniem informacji - metody prywatne, zmienne obiektowe)

- object(self)... end (this (J) - ale to zmienna związana, może się nazywać dowolnie)
- object inherit nadklasa as super... end (super(J) - ale ... j.w.)

- object (self : 'a) constraint 'a = ... end - ograniczenia na typ self wpływają na typ klasy  

{< x = inne_x; ... >} - tworzenie "zmodyfikowanego klona" prawdziwej "mojej" klasy ( this.clone()+modyfikacje (J) )

- brak null :)

- class ['a] klasa = ...  - klasa polimorficzna względem 'a (class Klasa<E>{...} (J))

- method m : 'b. 'b -> ... - metoda polimorficzna (niezależnie od klasy) ( <T> Typ<T> metoda(Typ<T> a){...} (J))


*)