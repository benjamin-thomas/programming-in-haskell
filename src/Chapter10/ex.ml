type action =
    | New_char of char
    | Back_space


let example : action list =
    [ New_char 'H'
    ; New_char 'e'
    ; New_char 'l'
    ; New_char 'l'
    ; New_char 'o'
    ; Back_space
    ; Back_space
    ; New_char 'p'
    ]


let execute : action -> unit = function
    | New_char c -> print_char c
    | Back_space -> print_string "\b"; print_char ' '; print_string "\b"


let generate : unit -> action Seq.t =
    Seq.unfold @@ fun () ->
        match input_char stdin with
            | '\n' -> None
            | c    -> Some (New_char c, ())


(*

utop # run @@ List.to_seq example;;
Help- : unit = ()

utop # run @@ generate ();;
Help;;
Help;;- : unit = ()

*)
let run (seq: action Seq.t) : char list =
    List.rev @@
        Seq.fold_left
            (fun acc action ->
              match action with
              | New_char c -> execute action; c :: acc
              | Back_space -> execute action; List.drop 1 acc
            )
            []
            seq
