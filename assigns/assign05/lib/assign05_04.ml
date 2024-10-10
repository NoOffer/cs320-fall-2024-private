type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

module ListSet = struct
    type t = int list

    let rec mem (x : int) (s : int list) : bool =
      match s with
      | []   -> false
      | h::t -> if (x = h) then true
        else mem (x) (t)

    let empty : int list = []

    let singleton (n : int) : int list = [n]

    let rec card (s : int list) : int =
      match s with
      | []   -> 0
      | _::t -> (card (t)) + 1

    let union (s1 : int list) (s2 : int list) : int list =
      let rec insert_sorted (curr : int list) (s : int) : int list =
        match curr with
        | []   -> [s]
        | h::t -> 
          if (s = h) then (curr)
          else if (s < h) then (s::curr)
          else h::(insert_sorted (t) (s))
      in
      let rec iter_int_list (curr : int list) (il : int list) : int list =
        match il with
        | []   -> curr
        | h::t -> iter_int_list (insert_sorted (curr) (h)) (t)
      in
      iter_int_list (iter_int_list ([]) (s1)) (s2)
end

module FuncSet = struct  
    type t = set_info

    let mem (x : int) (s : set_info ) : bool = s.ind (x)

    let empty : set_info = { ind = (fun (_ : int) -> false) ; mn = 0 ; mx = 1 ; }

    let singleton n = { ind = (fun (x : int) -> (x = n)) ; mn = n ; mx = n ; }

    let card (s : set_info) : int =
      let rec count_by_test (curr : int) (i : int) : int =
        if (i > s.mx) then curr
        else if (s.ind (i)) then count_by_test (curr + 1) (i + 1)
        else count_by_test (curr) (i + 1)
      in
      count_by_test (0) (s.mn)

    let union (s1 : set_info ) (s2 : set_info ) : set_info = 
    {
      ind = (fun (x : int) -> ((s1.ind (x)) || (s2.ind (x))));
      mn = (min (s1.mn) (s2.mn));
      mx = (max (s1.mx) (s2.mx));
    }
end