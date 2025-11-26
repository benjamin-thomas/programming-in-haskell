(*

while true;do utop -init ./tic_tac_toe.ml;clear;sleep 0.1;done

*)

let size = 3

type player =
    | B
    | X
    | O

let string_of_player = function
    | B -> "B"
    | X -> "X"
    | O -> "O"

type grid = player list list

let replicate n a = List.init n (fun _ -> a)

let empty : grid =
    replicate size @@ replicate size B

type 'a tree
    = Node of ('a * 'a tree list)


let tree_of_list lst =
    match List.rev lst with
        | []    -> failwith "empty list"
        | x::xs ->
            List.fold_left
                (fun acc x ->
                    Node (x, [acc])
                )
                (Node (x, []))
                xs

let map_both f g ( a, b ) =
    ( f a
    , g b
    )

let split_at n lst =
    map_both
        List.rev
        List.rev
    @@ snd
    @@ List.fold_left
           (fun ( m, (ls, rs) ) x ->
                ( m - 1
                , if m <= 0 then
                      ( ls, x :: rs )
                  else
                      ( x :: ls, rs )
                )
           )
           ( n, ( [], [] ) )
           lst

let valid g i =
    List.for_all
        Fun.id
        [ i >= 0
        ; i < size*size
        ; B == List.nth (List.concat g) i
        ]

let rec chop n lst =
    assert (n > 0);
    match split_at n lst with
        | ( ls, [] ) -> ls :: []
        | ( ls, rs ) -> ls :: chop n rs

let move g i p =
    match split_at i (List.concat g) with
        | ( xs, B :: ys ) when valid g i ->
            [ chop size (xs @ [p] @ ys) ]
        | _ -> []

        
(*
*Main Text.Pretty.Simple Data.List> transpose [[1,2,3], [4,5,6]]
[[1,4],[2,5],[3,6]]

NOTE: this OCaml version *REQUIRES* all lists to have the same length
utop # transpose [[1;2;3];[4;5;6];[7;8;9]];;
- : int list list = [[1; 4; 7]; [2; 5; 8]; [3; 6; 9]]

*)
let rec transpose = function
    | []      -> []
    | [] :: _ -> [] (* following the List.tl operation *)
    | rows    ->
        List.map List.hd rows
            :: transpose (List.map List.tl rows)


(*
*Main Text.Pretty.Simple Data.List> diag [[O,B,B], [B,O,B], [B,B,X]]
[O,O,X]

utop # diag [[O;B;B]; [B;O;B]; [B;B;X]];;
- : player list = [O; O; X]
*)
let diag =
    List.mapi (fun i row -> List.nth row i)

let wins p g =
    let line = List.for_all ((==) p) in
    let rows = g in
    let cols = transpose g in
    let dias = [ diag g; diag (List.map List.rev g) ] in
    List.exists line (rows @ cols @ dias)

let won g = wins O g || wins X g

let full grid =
   grid
   |> List.concat
   |> List.for_all ((<>) B)

let moves (g:grid) (p:player) : grid list =
    if  won g then [] else
    if full g then [] else
    (* List.concat @@ List.mapi (fun i _ -> move g i p) g *)
    List.concat @@ List.init (size*size) (fun i -> move g i p)

let next = function
    | O -> X
    | X -> O
    | B -> B

let rec game_tree (g:grid) (p:player) : grid tree =
    Node ( g
         , List.map
               (fun g' -> game_tree g' (next p))
               (moves g p)
         )


let cls () =
    print_string "\x1b[2J"

let goto (x,y) =
    ()
    ; print_string "\x1b["
    ; print_int x
    ; print_char ';'
    ; print_int y
    ; print_char 'H'

let beside = function
    | [] -> []
    | x::xs ->
          List.fold_left
              (List.map2 (^))
              x
              xs

let rec interleave sep = function
    | []    -> []
    | [x]   -> [x]
    | x::xs -> x :: sep :: interleave sep xs


let show_player = function
    | O -> ["   "; " O "; "   "]
    | X -> ["   "; " X "; "   "]
    | B -> ["   "; "   "; "   "]

let show_row (ps: player list) : string list =
    let bar = replicate 3 "|" in
    beside @@ interleave bar @@ List.map show_player ps

let put_grid g : unit =
    let bar = [String.make ((size*4)-1) '-'] in
    let unlines xs = List.fold_left (fun acc x -> x ^ acc) "" @@ List.map (fun x -> x ^ "\n") xs in
    print_endline @@ unlines @@ List.rev @@ List.concat @@ interleave bar @@ List.map show_row g


let prompt p =
    "Player " ^ string_of_player p ^ ", enter your move: "

let rec get_nat prompt =
    ()
    ; print_string prompt
    ; match read_int_opt () with
          | Some n -> n
          | None   ->
              ()
              ; print_endline "ERROR: invalid number"
              ; get_nat prompt


let rec run (g:grid) (p:player) : unit =
    ()
    ; cls ()
    ; goto (1,1)
    ; put_grid g
    ; run' g p
and run' (g:grid) (p:player) : unit =
    if wins O g then print_endline "Player O wins!" else
    if wins X g then print_endline "Player X wins!" else
    if   full g then print_endline "It's a draw!"   else
    let i = get_nat (prompt p) in
    match move g i p with
        | [g'] ->
            run g' (next p)
        | _    ->
            ()
            ; print_endline "ERROR: invalid move"
            ; run g p

(* [prune] is rather useless in OCaml, due to strict evaluation.
   Favor the usage of [game_tree_depth] for a similar effect
*)
let rec prune n = function
    | Node (x,  _) when n = 0 -> Node (x, [])
    | Node (x, []) -> Node (x, [])
    | Node (x, ts) ->
        Node ( x
             , List.map (prune (n-1)) ts 
             )


let rec game_tree_depth depth (g:grid) (p:player) : grid tree =
      Node ( g
           , if depth = 0 then
                []
             else
                 List.map
                     (fun g' -> game_tree_depth (depth-1) g' (next p))
                     (moves g p)
           )

