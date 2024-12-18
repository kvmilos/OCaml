open Base
open Core
open Lib.Sort

let%test_unit "IntSortT" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int list]
    (Quickcheck.Generator.list (Int.gen_incl Int.min_value Int.max_value))
    ~f:(fun lst ->
      let sorted = sort lst in
      let expected = List.sort ~compare:Int.compare lst in
      [%test_eq: int list] sorted expected)

let%test_unit "IntSortF" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: int list]
    (Quickcheck.Generator.list (Int.gen_incl Int.max_value Int.min_value))
    ~f:(fun lst ->
      let sorted = sort lst in
      let expected = List.sort ~compare:Int.compare lst in
      [%test_eq: int list] sorted expected)

let%test_unit "StringSortT" =
  Quickcheck.test
    ~sexp_of:[%sexp_of: string list]
    (Quickcheck.Generator.list String.quickcheck_generator)
    ~f:(fun lst ->
      let sorted = sort lst in
      let expected = List.sort ~compare:String.compare lst in
      [%test_eq: string list] sorted expected)

let%test_unit "StringSortF" =
Quickcheck.test
  ~sexp_of:[%sexp_of: string list]
  (Quickcheck.Generator.list String.quickcheck_generator)
  ~f:(fun lst ->
    let sorted = sort lst in
    let expected = List.rev (List.sort ~compare:String.compare lst) in
    [%test_eq: string list] sorted expected)

let%test_unit "CharSortT" =
Quickcheck.test
  ~sexp_of:[%sexp_of: char list]
  (Quickcheck.Generator.list Char.quickcheck_generator)
  ~f:(fun lst ->
    let sorted = sort lst in
    let expected = List.sort ~compare:Char.compare lst in
    [%test_eq: char list] sorted expected)

let%test_unit "CharSortF" =
Quickcheck.test
  ~sexp_of:[%sexp_of: char list]
  (Quickcheck.Generator.list Char.quickcheck_generator)
  ~f:(fun lst ->
    let sorted = sort lst in
    let expected = List.rev (List.sort ~compare:Char.compare lst) in
    [%test_eq: char list] sorted expected)