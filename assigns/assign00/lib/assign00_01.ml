let sqrt (n : int) : int = 
  let rec sqrt_rec (i : int) : int =
    if i * i >= n then i
    else sqrt_rec (i + 1)
  in
  sqrt_rec (0)

