(*----------------------------------------------------------------------------*
 # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi
 preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo
 izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike
 zafiksiramo tip `direction`:
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

(*----------------------------------------------------------------------------*
 ## Implementacija trakov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
 - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
 - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter
 pod njim z `^` označi mesto glave;
 - `read`, ki vrne znak pod glavo;
 - `write`, ki pod glavo zapiše dani znak;
 - `move`, ki glavo premakne v dano smer.

 Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita
 nespremenjenega.

 Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato
 poskrbite tako za učinkovitost kot za preglednost kode.
[*----------------------------------------------------------------------------*)

module type TAPE = sig
  type t

  val make : string -> t
  val print : t -> unit
  val read : t -> char
  val move : direction -> t -> t
  val write : char -> t -> t
end

module Tape : TAPE = struct
  type t = char list * char list


  let string_to_list (str : string) : char list = str |> String.to_seq |> List.of_seq

  let list_to_string lst : string = lst |> List.to_seq |> String.of_seq

  let delete_whitespace lst =
    let rev = List.rev lst in
    let rec aux l =
    match l with
    | [] -> []
    | x :: xs -> if x = ' ' then aux xs else List.rev l
    in aux rev

  let make (str : string) : t = ([], string_to_list str)
  let print (tape : t) : unit =
    let str1 =  list_to_string ( List.rev (delete_whitespace (fst tape))) in
    let str2 = list_to_string (delete_whitespace (snd tape)) in
    let index = String.length str1 in
    print_endline (str1 ^ str2);
    print_endline (String.make index ' ' ^ "^")
  let read (tape : t) : char =
    match tape with
    | ([], []) -> ' '
    | (x :: xs, []) -> ' '
    | ( _ , y :: ys) -> y
  let move (mov : direction) (tape : t) : t =
    match mov with
    | Right ->
      (match tape with
      | ([], []) -> ([], [])
      | (x :: xs, []) -> (' ' :: x :: xs, [])
      | ([], y :: ys) -> ([y], ys)
      | (x :: xs, y :: ys) -> (y :: x :: xs, ys))
    | Left ->
      (match tape with 
      | ([], []) -> ([], [])
      | (x :: xs, []) -> (xs, [x])
      | ([], y :: ys) -> ([],' ' :: y :: ys)
      | (x :: xs, y :: ys) -> (xs, x :: y :: ys))

  let write (chr : char) (tape : t) : t =
    match tape with
    | (_, []) -> (fst tape, [chr])
    | (_, x :: xs) -> (fst tape, chr :: xs)
end

let primer_trak = Tape.(
  make "ABCDE"
  |> move Left
  |> move Left
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> write '!'
  |> print
)
(*
AB!DE
  ^
*)
(* val primer_trak : unit = () *)

(*----------------------------------------------------------------------------*
 ## Implementacija Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip Turingovih strojev;
 - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih
 stanj ter prazno prehodno funkcijo;
 - `initial`, ki vrne začetno stanje stroja;
 - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto
 (q', a', d)$;
 - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.

 Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa
 naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko
 predpostavite, da ju bomo klicali le na poprej podanih stanjih.

 Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost
 kode.
[*----------------------------------------------------------------------------*)

module type MACHINE = sig
  type t
  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
end

module Machine : MACHINE = struct
  type t = { init_s : state ; state : state; states : state list; transitions : ((state * char) * (state * char * direction)) list}
  let make s sl = {init_s = s; state = s; states = sl; transitions = []}
  let initial t = t.init_s
  let add_transition in_s in_char out_s out_char dir t = 
    {t with transitions = ((in_s, in_char), (out_s, out_char, dir)) :: t.transitions}
  let step t in_s tape = 
    let in_c = Tape.read tape in
    match List.assoc_opt (in_s, in_c) t.transitions with
    | None -> None
    | Some (out_s, out_c, dir) -> 
      let new_t = Tape.write out_c tape |> Tape.move dir in
      Some (out_s, new_t)
end

(*----------------------------------------------------------------------------*
 Primer stroja "Binary Increment" na <http://turingmachine.io> lahko
 implementiramo kot:
[*----------------------------------------------------------------------------*)

let binary_increment =
  Machine.(
    make "right" [ "carry"; "done" ]
    |> add_transition "right" '1' "right" '1' Right
    |> add_transition "right" '0' "right" '0' Right
    |> add_transition "right" ' ' "carry" ' ' Left
    |> add_transition "carry" '1' "carry" '0' Left
    |> add_transition "carry" '0' "done" '1' Left
    |> add_transition "carry" ' ' "done" '1' Left
  )

(* val binary_increment : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki
 simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz.
 Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše
 le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti
 izvajanja.
[*----------------------------------------------------------------------------*)


let slow_run (tur : Machine.t) str = 
  let tape = Tape.make str in
  let rec loop s tape =
    Tape.print tape; print_endline s;
    if s = "done" then () else
    match Machine.step tur s tape with
    | None -> print_endline "Illegal instruction"
    | Some (s, tape) -> loop s tape
    in loop (Machine.initial tur) tape

let primer_slow_run =
  slow_run binary_increment "1011"
(*
1011
^
right
1011
  ^
right
1011
  ^
right
1011
    ^
right
1011
    ^
right
1011
    ^
carry
1010
  ^
carry
1000
  ^
carry
1100
^
done
*)
(* val primer_slow_run : unit = () *)


let speed_run (tur : Machine.t) str = 
  let tape = Tape.make str in
  let rec loop s tape =
    if s = "done" then (Tape.print tape; print_endline s;) else
    match Machine.step tur s tape with
    | None -> print_endline "Illegal instruction"
    | Some (s, tape) -> loop s tape
    in loop (Machine.initial tur) tape

let primer_speed_run =
  speed_run binary_increment "1011"
(*
1100
^
*)
(* val primer_speed_run : unit = () *)

(*----------------------------------------------------------------------------*
 ## Krajši zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih,
 nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak
 in stanje pa pustimo pri miru. Zapišite funkcije:

 - `for_state`
 - `for_character`
 - `for_characters`
 - `move`
 - `switch_and_move`
 - `write_and_move`
 - `write_switch_and_move`

 s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj.
 Implementacijo in tipe ugotovite sami.
[*----------------------------------------------------------------------------*)

let for_state s transitions machine =
  List.fold_left (fun tur transition -> transition s tur) machine transitions

let for_character char transition state machine =
  transition state char machine

let for_characters str transition s machine =
  String.fold_left
    (fun tur char -> transition s char tur)
    machine str

let move dir s char machine =
  Machine.add_transition s char s char dir machine

let switch_and_move new_s dir old_s char machine =
  Machine.add_transition old_s char new_s char dir machine

let write_and_move new_char dir =
  fun s char machine ->
    Machine.add_transition s char s new_char dir machine

let write_switch_and_move new_char new_s dir =
  fun state character machine ->
    Machine.add_transition state character new_s new_char dir machine

let binary_increment' =
  Machine.make "right" ["carry"; "done"]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "carry" Left
  ]
  |> for_state "carry" [
    for_character '1' @@ write_and_move '0' Left;
    for_characters "0 " @@ write_switch_and_move '1' "done" Left
  ]
(* val binary_increment' : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 ## Primeri Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na
 različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni
 niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu
 izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo
 trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku,
 kot se je začel prvotni niz.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Obračanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki začetni niz obrne na glavo.
[*----------------------------------------------------------------------------*)

let reverse =
  Machine.make "start" ["1"; "0"; "search"; "delete"; "done"; "move"]
  |> for_state "start" [
    for_character '1' (move Right);
    for_character '0' (move Right);
    for_character ' ' (switch_and_move "search" Left)
  ]
  |> for_state "search" [
    for_character '1' (write_switch_and_move 'x' "1" Right);
    for_character '0' (write_switch_and_move 'x' "0" Right);
    for_character 'x' (move Left);
    for_character ' ' (write_switch_and_move ' ' "delete" Right)
  ]
  |> for_state "1" [
    for_character '1' (move Right);
    for_character '0' (move Right);
    for_character 'x' (move Right);
    for_character ' ' (write_switch_and_move '1' "move" Left)
  ]
  |> for_state "0" [
    for_character '1' (move Right);
    for_character '0' (move Right);
    for_character 'x' (move Right);
    for_character ' ' (write_switch_and_move '0' "move" Left)
  ]
  |> for_state "delete" [
    for_character 'x' (write_and_move ' ' Right);
    for_characters "01" (switch_and_move "done" Left)
  ]
  |> for_state "move" [
    for_character 'x' (switch_and_move "search" Left);
    for_characters "01" (move Left)
  ]

let primer_reverse = speed_run reverse "0000111001"

(* 
1001110000          
^
*)
(* val primer_reverse : unit = () *)

(*----------------------------------------------------------------------------*
 ### Podvajanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki podvoji začetni niz.
[*----------------------------------------------------------------------------*)


let duplicate = 
  Machine.make "start" ["1"; "0"; "search"; "take"; "again1"; "again0"; "done"]
  |> for_state "start" [
    for_characters "01" (move Right);
    for_character ' ' (write_switch_and_move 'x' "search" Left)
  ]
  |> for_state "search" [
    for_characters "01x" (move Left);
    for_character ' ' (switch_and_move "take" Right)
  ]
  |> for_state "take" [
    for_character '1' (write_switch_and_move ' ' "1" Right);
    for_character '0' (write_switch_and_move ' ' "0" Right)
  ]
  |> for_state "1" [
    for_characters "01x" (move Right);
    for_character ' ' (write_switch_and_move '1' "again1" Right)
  ]
  |> for_state "0" [
    for_characters "01x" (move Right);
    for_character ' ' (write_switch_and_move '0' "again0" Right)
  ]
  |> for_state "again1" [
    for_character ' ' (write_switch_and_move '1' "search" Left)
  ]
  |> for_state "again0" [
    for_character ' ' (write_switch_and_move '0' "search" Left)
  ]
  |> for_state "take" [
    for_character '0' (write_switch_and_move ' ' "0" Right);
    for_character '1' (write_switch_and_move ' ' "1" Right);
    for_character 'x' (write_switch_and_move ' ' "done" Right)
  ]


let primer_duplicate = speed_run duplicate "010011"

(* 
001100001111       
^
*)
(* val primer_duplicate : unit = () *)

(*----------------------------------------------------------------------------*
 ### Eniški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano
 v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.
[*----------------------------------------------------------------------------*)

let to_unary = 
  Machine.make "start" ["1"; "0"; "right"; "delete"; "findx"; "increment"; "done"; "left"]
  |> for_state "start" [
    for_characters "01" (move Right);
    for_character ' ' (write_switch_and_move 'x' "decrement" Left)
  ]
  |> for_state "decrement" [
    for_character '1' (write_switch_and_move '0' "addition" Right);
    for_character '0' (write_and_move '1' Left);
    for_character ' ' (switch_and_move "delete" Right)
  ]
  |> for_state "delete" [
    for_characters "01" (write_and_move ' ' Right);
    for_character 'x' (write_switch_and_move ' ' "done" Right)
  ]
  |> for_state "addition" [
    for_characters "01x" (move Right);
    for_character ' ' (write_switch_and_move '1' "findx" Left)
  ]
  |> for_state "findx" [
    for_characters "01" (move Left);
    for_character 'x' (switch_and_move "decrement" Left)
  ]


let primer_to_unary = speed_run to_unary "1010"

(* 
1111111111
^
*)
(* val primer_to_unary : unit = () *)

(*----------------------------------------------------------------------------*
 ### Dvojiški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
 sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v
 dvojiškem zapisu.
[*----------------------------------------------------------------------------*)


let to_binary =
  Machine.make "start" ["1"; "0"; "right"; "delete"; "findx"; "increment"; "done"; "left"]
  |> for_state "start" [
    for_characters "01" (move Left);
    for_character ' ' (write_switch_and_move 'x' "left" Left)
  ]
  |> for_state "left" [
    for_character ' ' (write_switch_and_move '0' "right" Right)
  ]
  |> for_state "right" [
    for_characters "01x" (move Right);
    for_character ' ' (switch_and_move "delete" Left)
  ]
  |> for_state "delete" [
    for_characters "01" (write_switch_and_move ' ' "findx" Left);
    for_character 'x' (write_switch_and_move ' ' "done" Left)
  ]
  |> for_state "findx" [
    for_character 'x' (switch_and_move "increment" Left);
    for_characters "01" (move Left)
  ] 
  |> for_state "increment" [
    for_character '1' (write_and_move '0' Left);
    for_characters "0 " (write_switch_and_move '1' "right" Right);
  ]

let primer_to_binary = speed_run to_binary (String.make 42 '1')

(* 
101010                                           
^
*)
(* val primer_to_binary : unit = () *)
