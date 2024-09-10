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

let nth_prime (n : int) : int =
    let rec nth_prime_impl (curr : int) (n : int) : int =
        if n = 0 then curr
        else
            if is_prime (curr + 2) then nth_prime_impl (curr + 2) (n - 1)
            else nth_prime_impl (curr + 2) (n)
    in
    if n = 0 then 2
    else nth_prime_impl (3) (n - 1)

let nth (s : int) (i : int) : int =
  let p = nth_prime (i)
  in
  let rec nth_impl (s : int) (count : int) : int =
    if (s = 0) || (s mod p <> 0) then count
    else nth_impl (s / p) (count + 1)
  in
  nth_impl (s) (0)

let rec pow (n : int) (k : int) : int =
    if k = 0 then 1
    else (pow (n) (k - 1)) * n

let to_string (s : int) : string =
  if s < 2 then "[]"
  else
    let rec to_sring_impl (s : int) (i : int) (curr : string) =
      let decoded = nth (s) (i)
      in
      if decoded = 0 then curr ^ "]"
      else
        if (String.length (curr)) = 0 then to_sring_impl (s / (pow (nth_prime (i)) (decoded))) (i + 1) ("[" ^ (string_of_int (decoded)))
        else to_sring_impl (s / (pow (nth_prime (i)) (decoded))) (i + 1) (curr ^ "; " ^ (string_of_int (decoded)))
    in
    to_sring_impl (s) (0) ("")