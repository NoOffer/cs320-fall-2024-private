type dir = 
| North
| South
| East
| West

type path = dir list

let move (d : dir) (pos : float * float) : (float * float) =
    let (x, y) = pos
    in
    match d with
    | North -> (x, y +. 1.)
    | South -> (x, y -. 1.)
    | East  -> (x +. 1., y)
    | West  -> (x -. 1., y)

let dist (p : path) : float =
    let rec move_along_path (p : path) (pos : float * float) : (float * float) =
        match p with
        | []   -> pos
        | h::t -> move_along_path (t) (move (h) (pos))
    in
    let (x, y) = move_along_path (p) (0., 0.)
    in
    sqrt (x *. x +. y *. y)
