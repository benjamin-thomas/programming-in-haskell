(*

cd ./src/Chapter08/Part07/
dune exec ./abstract_machine.exe

*)

type op =
  | EVAL of expr
  | ADD of int

and expr =
  | Val of int
  | Add of expr * expr

let rec eval expr cont =
  match expr with
  | Val n -> exec cont n
  | Add (x, y) -> eval x (EVAL y :: cont)

and exec cont n =
  match cont with
  | [] -> n
  | EVAL y :: rest -> eval y (ADD n :: rest)
  | ADD m :: rest -> exec rest (n + m)

let value e = eval e []

let () =
  let
    expr = Add
            ( Add
                ( Val 2
                , Val 3
                )
            , Val 4
            )
  in
  let result = value expr in
  assert(9 = result);
  Printf.printf "%d\n" result
