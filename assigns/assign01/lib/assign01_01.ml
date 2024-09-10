let rec pow (n : int) (k : int) : int =
    if k = 0 then 1
    else (pow (n) (k - 1)) * n
