let czy_pierwsza pierwsze x = 
  let rec sprawdz pierwsze limit kolejny = 
    match pierwsze with
    | [] -> sprawdz_dalej limit kolejny
    | p :: ps -> 
        if p > limit then true
        else if x mod p = 0 then false
        else sprawdz ps limit kolejny
  and sprawdz_dalej limit kandydat = 
    if kandydat > limit then true
    else if x mod kandydat = 0 then false
    else sprawdz_dalej limit (kandydat + 1)
  in
  let sqrt_x = Float.to_int @@ Float.sqrt @@ Float.of_int x in
  let ostatnia = List.fold_left (fun _ x -> x) 0 pierwsze in
  let kolejny = ostatnia + 1 in
  sprawdz pierwsze sqrt_x kolejny