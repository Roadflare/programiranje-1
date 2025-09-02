(*
  Deli in Vladaj
*)

(*
  Hitro Izbiranje
*)

let rec quickselect (k: int) (lst: 'a list) =
  (*
    Funkcija sprejme (premesan) list in iz njega vzame prvi element kot pivot. Okoli njega razdeli seznam na dva dela.
    Ce je zahtevana velikost enaka dolzini levega seznama - 1 vrnemo kar pivot, drugace izvajamo postopek na podseznamih,
    glede na njihovo dolzino in velikost K-ja.
    Najboljsa casovna zahtevnost je O(n), kjer je prvi element, to kar ze iscemo. Particija seznama vzame O(n) casa.
    Povprecna casovna zahtevnost je O(n log(n)), kjer je prvotni seznam razdeljen priblizno na dva enaka dela.
    Najslabsa casovna zahtevnost je O(n²), kjer je izbrani pivot vedno najmanjsi/najvecji element v seznamu.
  *)
  match lst with
  | [] -> None
  | x :: xs ->
    let lstL, lstR = List.partition (fun y -> y < x) xs in
    let n = List.length lstL in
    if k - 1 = n then Some x
    else if n > k - 1 then quickselect k lstL
    else quickselect(k - n - 1) lstR

(*
  Najblizji Par Tock v Ravnini
*)

type point = float * float

let distance (pt1 : point) (pt2: point) : float =
(*
Evklidska razdalja med tockama, koren opustim ker je monotona narascajoca funkcija in ne unici urejenosti.
Casovna zahtevnost je O(1), saj vse operacije (sestevanje, mnozenje, fst, snd) vzamejo O(1) casa.
*)
  (fst pt1 -. fst pt2) *. (fst pt1 -. fst pt2) +. (snd pt1 -. snd pt2) *. (snd pt1 -. snd pt2)

let divide(arr) =
  (*
  Razdeli array po sredini x koordinat, ce je ta po njih urejen.
  Casovna zahtevnost je O(n), kjer je n dolzina seznama, zaradi ustvarjanja polovicnih arrayov ex. n = n/2 + n/2.
  *)
  let len = Array.length arr in
  let arrL = Array.sub arr 0 (len / 2) in
  let arrR = Array.sub arr (len / 2) (len - (len / 2)) in
  arrL, arrR

let smallest_distance (arr: point array) : float * (point * point) =
  (*
  Funkcija izracuna najkrajso razdaljo med tockami in vrne razdaljo in ti dve tocki.
  Casovna zahtevnost je O(n²) zaradi dvojnih for-loopov.
  *)
  let n = Array.length arr in
  let min = ref Float.infinity in
  let points = ref ((0.0, 0.0), (0.0, 0.0)) in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let cur = distance arr.(i) arr.(j) in
        if cur < !min then
          (
          min := cur; points := (arr.(i), arr.(j))
          )
    done
  done;
  (!min, !points)

let filter_array (f : ('a -> bool)) (arr : 'a array) : 'a array =
  (*
  Funkcija deluje kot List.filter vendar prepisana na za Arraje
  Casovna zahtevnost je O(n) zaradi Array.of_list in Array.fold_right ki iterirata preko n elementov.
  *)
  let filtered = Array.fold_right (fun x acc ->
    if f x then
      x :: acc
    else
      acc
  ) arr [] in
  Array.of_list filtered
  
let main(arr: point array) =
  (*
      Casovna zahtevnost je O(n log(n)) zaradi sortiranja (heapsort) in metode divide and conquer pri racunanju razdaje na 
      polovicnih seznamih (smallest_distance) namesto na vseh tockah hkrati.
  *)
  Array.sort (fun x y -> compare (fst x) (fst y)) arr;

  let arrL, arrR = divide arr in
  let left = smallest_distance arrL in
  let right = smallest_distance arrR in
  
  let min, point_min = if fst left < fst right then left else right in
  let min_x = fst arrR.(0) -. min in
  let max_x = fst arrR.(0) +. min in

  let sorted_arr = filter_array (fun (x, _) -> x >= min_x && x <= max_x) arr in
  let dist = smallest_distance sorted_arr in
  if fst dist < min then snd dist else point_min

(*
  Dinamicno Programiranje
*)

(*
  Red, Green, Blue tiles
  https://projecteuler.net/problem=117
*)

let rec tiling n =
  (*
    Rocno vnesel base cases na zacetna mesta v array. Ostala mesta izracunam s prejsnjimi 4imi.
    Casovna zahtevnost je O(n), zaradi for-loopa, ki napolni seznam dolzine n.
  *)
  let arr = Array.make (n + 1) (-1) in
  arr.(0) <- 1;
  arr.(1) <- 1;
  arr.(2) <- 2;
  arr.(3) <- 4;
  for i = 4 to n do
    if arr.(i) = -1 then
      arr.(i) <- arr.(i - 1) + arr.(i - 2) + arr.(i - 3) + arr.(i - 4);
  done;
  arr.(n)

(*
  Path Sum: Three ways
  https://projecteuler.net/problem=82
*)

let minimal_path matrix =
  (*
    Ustvarim kopijo Matrike in vanjo shranjujem najkrajso pot do tistega mesta. Nakoncu preberem iz zadnjega stolpca resitev.
    Casovna zahtevnost programa je O(n * m), zaradi kopiranja, kjer sta n in m dimenziji matrike.
  *)
  let rows = Array.length matrix in
  let cols = Array.length matrix.(0) in

  let dp = Array.map (Array.copy) matrix in

  for j = 1 to cols - 1 do
    for i = 0 to rows - 1 do
      let left = dp.(i).(j - 1) in
      let up = if i > 0 then dp.(i - 1).(j) else max_int in
      let down = if i < rows - 1 then dp.(i + 1).(j) else max_int in
      dp.(i).(j) <- dp.(i).(j) + min left (min up down)
    done
  done;

  let min_sum = ref max_int in
  for i = 0 to rows - 1 do
    min_sum := min !min_sum dp.(i).(cols - 1)
  done;
  !min_sum

(*
  Maximum Path Sum 2
  https://projecteuler.net/problem=67
*)


let max_path_sum triangle =
  (*
    Funkcija sprejme trikotnik (int list list) in naredi njegovo kopijo v katero si shranjuje najdajse vsote do mesta.
    Pomozna funkcija izvaja rekurzivne klice in belezi najdaljso pot do i-j mesta.
    Casovna zahtevnost programa je O(2^n), saj ima vsak rekurzivni klic dve moznosti, leva in desna, kjer je n stevilo vrstic.
  *)
  let rows = List.length triangle in
  let triangle_copy = Array.of_list (List.map Array.of_list triangle) in
  
  let rec aux i j =
    if i = rows - 1 then
      triangle_copy.(i).(j)
    else
      let left = aux (i + 1) j in
      let right = aux (i + 1) (j + 1) in
      triangle_copy.(i).(j) + max left right
    in aux 0 0

(*
  Coin Sums
  https://projecteuler.net/problem=31
*)

let rec coin_sum n coins =
  (*
    Funkcija sprejme stevilo in mozne kovance. Ce je seznam z moznimi kovanci prazen in n = 0 vrne 1 (base case) drugace 0.
    Ce je seznam z kovanci neprazen jih razdeli na trenutnega in ostale, zopet če je n < 0 vrne 0. Ce je vse vredu, locimo primera,
    kjer uporabimo trenutni kovanec (coin_sum (n - coin) coins) in kjer ga izpustimo (coin_sum n rest). S tem se izognemo veckratnemu 
    prestevanju.
    Casovna zahtevnost je O(2^k), kjer je k stevilo kovancev. To je zaradi moznosti da nek kovanec uporabimo ali zavrzemo.
  *)
  match coins with
  | [] -> if n = 0 then 1 else 0
  | coin :: rest -> 
      if n < 0 then 0
      else (coin_sum (n - coin) coins) + (coin_sum n rest)

let coins = [200; 100; 50; 20; 10; 5; 2; 1]
