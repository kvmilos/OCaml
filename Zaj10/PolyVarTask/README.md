# Zadanie
Dany jest interpreter (nieco sztucznego ;) języka podstawowych wyrażeń i jego kompilator do podstawowej Maszyny Stosowej (basic).

Używając technik i sztuczek stosowanych na zajęciach rozszerz go do interpretera rozszerzonego języka i jego kompilatora do rozszerzonej Maszyny Stosowej (ext). Przy rozpatrywaniu wariantów wszędzie tam gdzie to możliwe wywołuj funkcje dla języka podstawowego, unikaj przepisywania kodu (poza funkcjami generycznymi takimi jak `print_ext_expr`, `run_ext` itp. - na takich nam w tym ćwiczeniu nie zależy).

Moduł checker zawiera wywołanie funkcji testujących z obu modułów (w miarę możliwości) na paru przykładach. Dodaj tam też jakieś własne przykłady.

Aby skompilować projekt należy wywołać oczywiście `dune build`, a po dopisaniu pliku `ext.ml` (lub tymczasowym wycięciu modułu `Ext` z `checker.ml`) należy wywołać polecenie `dune exec ./checker.exe`
