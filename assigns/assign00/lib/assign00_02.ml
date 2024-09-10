let sqrt (n : int) : int = 
    let rec sqrt_rec (i : int) : int =
        if i * i >= n then i
        else sqrt_rec (i + 1)
    in
    sqrt_rec (0)

let is_prime (n : int) : bool =
  if n < 2 then false
  else
    let rng_max : int = if n < 6 then n / 2 else sqrt (n)
    in
    let rec is_prime_rec (i : int) : bool =
      if i > rng_max then true
      else if n mod i = 0 then false
      else is_prime_rec (i + 1)
    in
    is_prime_rec (2)