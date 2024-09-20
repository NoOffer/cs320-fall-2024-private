type piece = 
| X
| O

type pos = 
| Piece of piece
| Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
| Top
| Middle
| Bottom

type col_index = 
| Left
| Middle
| Right

type pos_index = row_index * col_index

let get_pos (b: board) (p: pos_index) : pos =
    let get_pos_in_row (row: pos * pos * pos) (p: col_index) : pos =
        let (a, b, c) = row
        in
        match p with
        | Left   -> a
        | Middle -> b
        | Right  -> c
    in
    let (row_idx, col_idx) = p
    in
    let (a, b, c) = b
    in
    match row_idx with
    | Top     -> get_pos_in_row (a) (col_idx)
    | Middle  -> get_pos_in_row (b) (col_idx)
    | Bottom  -> get_pos_in_row (c) (col_idx)

let winner (b : board) : bool =
    let rec check_win_conditions (c : (pos_index * pos_index * pos_index) list) : bool =
        match c with
        | [] -> false
        | h::t -> let (p1, p2, p3) = h
            in
            let pp2 : pos = get_pos (b) (p2)
            in
            if get_pos (b) (p1) = pp2 && pp2 = get_pos (b) (p3) then true
            else check_win_conditions(t)
    in
    check_win_conditions ([
        ((Top,    Left),   (Top,    Middle), (Top,    Right) );
        ((Middle, Left),   (Middle, Middle), (Middle, Right) );
        ((Bottom, Left),   (Bottom, Middle), (Bottom, Right) );

        ((Top,    Left),   (Middle, Left),   (Bottom, Left)  );
        ((Top,    Middle), (Middle, Middle), (Bottom, Middle));
        ((Top,    Right),  (Middle, Right),  (Bottom, Right) );

        ((Top,    Left),   (Middle, Middle), (Bottom, Right) );
        ((Top,    Right),  (Middle, Middle), (Bottom, Left)  )
    ])
        