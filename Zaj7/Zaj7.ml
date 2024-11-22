type expr = 
| Int of int
| Plus of expr * expr
| Times of expr * expr
| Neg of expr
| Var of char

let parse s = 
  let rec pom stos i =
    if i >= String.length s then
      match stos with
      | [n] -> n
      | _ -> failwith "Blad"
    else
      if s.[i] >= '0' && s.[i] <= '9' then 
        pom ((Int (int_of_char s.[i] - int_of_char '0')) :: stos) (i+1)
      else match s.[i] with
        |'-' -> (match stos with
          | a :: t -> pom ((Neg (a)) :: t) (i+1)
          | _ -> failwith "Blad")
        |'+' -> (match stos with
          | a :: b :: t -> pom ((Plus (b, a)) :: t) (i+1)
          | _ -> failwith "Blad")
        |'*' -> (match stos with
          | a :: b :: t -> pom ((Times (b, a)) :: t) (i+1)
          | _ -> failwith "Blad")
        |_ -> failwith "Blad"
  in
  pom [] 0