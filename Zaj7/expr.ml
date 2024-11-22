type expr = 
| Int of int
| Plus of expr * expr
| Times of expr * expr
| Neg of expr
| Var of char

let parse (s : string) : expr =
  let rec pom stos i =
    if i >= String.length s then
      match stos with
      | [n] -> n
      | _ -> failwith "Blad"
    else
      match s.[i] with
      | '0'..'9' -> pom (Int (int_of_char s.[i] - int_of_char '0') :: stos) (i + 1)
      | 'a'..'z' -> pom (Var s.[i] :: stos) (i + 1)
      | '-' -> (match stos with
                | a :: t -> pom (Neg a :: t) (i + 1)
                | _ -> failwith "Blad")
      | '+' -> (match stos with
                | a :: b :: t -> pom (Plus (b, a) :: t) (i + 1)
                | _ -> failwith "Blad")
      | '*' -> (match stos with
                | a :: b :: t -> pom (Times (b, a) :: t) (i + 1)
                | _ -> failwith "Blad")
      | _ -> failwith "Blad"
  in
  pom [] 0

module CharMap = Map.Make(Char)

let rec calcf (f : char -> int) (e : expr) : int =
  match e with
  | Int n -> n
  | Var x -> f x
  | Plus (a, b) -> calcf f a + calcf f b
  | Times (a, b) -> calcf f a * calcf f b
  | Neg a -> -(calcf f a)

let calc (vars : int CharMap.t) (e : expr) : int =
  let f x = CharMap.find x vars in
  calcf f e