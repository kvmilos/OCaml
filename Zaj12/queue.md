# Zadanie
Zaimplementuj abstrakcyjną klasę `queue` udostępniającą metody:

`insert: int -> unit` - wstawiającą element do kolejki;

`get_min: int` - podającą najmniejszą wartość przechowywaną w kolejce;

`get_sum: int` - podającą sumę elementów przechowywanych w kolejce;

`get_size: int` - podającą ilość elementów przechowywanych w kolejce;

`remove_min: unit` - usuwającą najmniejszy element z kolejki.

A także dwie jej podklasy poprawnie implementujące powyższe funkcje tak aby funkcje get działały w czasie O(1) np: przy pomocy stosu (najmniejszy element znajduje się na wierzchu posortowanego stosu), drzewa BST lub kopca. W implementacji postaraj się unikać podwójnego pisania kodu (części wspólne kodów różnych kolejek mogą być obsłużone przez wspólną nadklasę).

Dodatkowo zaimplementuj funkcję `sort: queue -> list -> list` sortującą podaną listę przy pomocy kolejki. Funkcja `sort` powinna być niezależna od tego jakiego typu kolejkę dostanie.