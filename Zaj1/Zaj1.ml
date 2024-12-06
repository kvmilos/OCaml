(* Zad 0 *)
String.make (int_of_char 'A') (char_of_int (3 * 32 + 1))


(* Zad 1 *)
let cosdziesiec = (/) 10;;
(* cosdziesiec 20;; zwroci 0, bo dzielimy 10:int przez 20:int operacją dzielenia:int *)


(* Zad 2 *)
let rec fib_wyk x =
    if x <= 0 then 0
    else if x = 1 then 1
    else (fib_wyk (x-1) + fib_wyk (x-2));;

let rec fibpom i a b n =
    if i = n then b
    else fibpom (i+1) b (a+b) n;;

let fib_lin x = fibpom 1 0 1 x;;

(* to wyżej można też zapisać jako: *)
let fib_lin x =
    let rec fibpom i a b n =
        if i = n then b
        else fibpom (i+1) b (a+b) n
    in
    fibpom 1 0 1 x;;


(* Zad 3 *)
let rec st_parz x = 
    if x = 0 then -1
    else if x mod 2 <> 0 then 0 
    else 1 + st_parz (x/2);;

let st_parz_og x =
    let rec st_parz_pom x n =
        if x = 0 then -1
        else if x mod 2 <> 0 then n
        else st_parz_pom (x/2) (n+1)
    in
    st_parz_pom x 0;;


(* Zad 4 *)
(* nwd : int -> int -> int *)
let rec nwd a b =
    if b = 0 then a
    else nwd b (a mod b);;

let rec nwd_z_def_rown a b =
    if b = 0 then a
    else let a = b and b = a mod b
    in
    nwd a b;;


(* Zad 5 *)
(* power : int -> int -> int *)

(* poprzednia wersja - bez ogonowego *)

(* let rec power_lin x n =
    if n = 0 then 1
    else x * power_lin x (n-1);;

let rec power_log x n = 
    if n = 0 then 1
    else if n mod 2 = 0 then 
        let power_half = power_log x (n/2) 
        in
        power_half * power_half
    else x * power_log x (n-1);; *)

(* wersja poprawiona - ogonowa *)

let power_lin x n =
    let rec power_lin_tail x n acc =
        if n = 0 then acc
        else power_lin_tail x (n-1) (acc * x)
    in
    power_lin_tail x n 1;;

let power_log x n = 
    let rec power_log_tail x n acc = 
        if n = 0 then acc
        else if n mod 2 = 0 then
            power_log_tail (x*x) (n/2) acc
        else
            power_log_tail x (n-1) (acc*x)
    in
    power_log_tail x n 1;;