(* ##1. Domaca Naloga

  1. Ogrevanje

  1.1 Stevke

*)

let rec stevke base n =
  match n with
  | a when a < base -> [n]
  | b -> stevke base (n / base) @ [n mod base]

(*
  1.2 Zacetek Seznama
*)

let rec take n l =
  match l with
  | [] -> []
  | x :: xs -> if n = 0 then [] else x :: take (n - 1) xs

(*
  1.3 Odstranjevanje Ujemajocih
*)

let rec drop_while f l =
  match l with
    | [] -> []
    | x :: xs -> if f x then drop_while f xs else l

(*
  1.4 filter_mapi
*)

let filter_mapi f l =
  let rec aux l i =
    match l with
    | [] -> []
    | x :: xs ->
      match f i x with
      | Some n -> n :: aux xs (i + 1)
      | None -> aux xs (i + 1)
in aux l 0

(*
  2. Izomorfizmi
*)

(*
  2.1
  A * B ≃ B * A
*)

let psi1 (a, b) = (b, a)
let phi1 (b, a) = (a, b)

(*
  2.2
  A + B ≃ B + A
*)

type ('a, 'b) sum = In1 of 'a | In2 of 'b

let phi2 = function
  | In1 x -> In2 x
  | In2 x -> In1 x

let psi2 = function
  | In2 x -> In1 x
  | In1 x -> In2 x

(*
  2.3
  A * (B * C) ≃ (A * B) * C
*)

let psi3 (a, (b, c)) = ((a, b), c)
let phi3 ((a, b), c) = (a, (b, c))

(*
  2.4
  A + (B + C) ≃ (A + B) + C
*)

let psi4 = function
  | In1 x -> In1 (In1 x )
  | In2 In1 x -> In1 (In2 x)
  | In2 In2 x -> In2 x

  let phi4 = function
    | In1 In1 x -> In1 x
    | In1 In2 x -> In2 (In1 x)
    | In2 x -> In2 (In2 x)

(*
  2.5
  A * (B + C) ≃ (A * B) + (A * C)
*)

let psi5 = function
  | (a, In1 b) -> In1 (a, b)
  | (a, In2 b) -> In2 (a, b)

let phi5 = function
  | In1 (a, b) -> (a, In1 b)
  | In2 (a, c) -> (a, In2 c)


(*
  2.6
  A^(B + C) ≃ A^B * A^C
*)

let psi6 alpha = ((fun x -> alpha (In1 x)), fun x -> alpha (In2 x))

let phi6 (a, b) = function
  | In1 x -> a x
  | In2 x -> b x

(*
  2.7
  (A * B)^C ≃ A^C * B^C
*)

let psi7 f = (fun x -> fst (f x), fun x -> snd (f x))

let phi7 (f, g) = function x -> (f x, g x)

(*
  3. Polinomi
*)

type polinom = int list

(*
  3.1 Odstranjevanje Odvecnih Nicel
*)

let rec pocisti poly =
  match List.rev poly with
  | [] -> []
  | 0 :: xs -> pocisti xs
  | x :: xs -> List.rev poly

(*
  3.2 Sestevanje
*)

let ( +++ ) x y =
  let rec aux x y sum =
    match x, y with
    | [], [] -> pocisti sum
    | x, [] -> pocisti (sum @ x)
    | [], y -> pocisti (sum @ y)
    | x :: xs, y :: ys -> aux xs ys (sum @ [x + y])
  in aux x y []

(*
  3.3 Mnozenje
*)

let rec ( *** ) x y =
  let rec aux x y acc =
  match x with
  | [] -> acc
  | b :: bs -> aux bs ([0] @ y) (( +++ ) (List.map (( * ) b) y) acc)
  in aux x y []

(*
  3.4 Izracun Vrednosti v tocki
*)

(* Formula: https://en.wikipedia.org/wiki/Exponentiation_by_squaring *)
let rec pow x n =
  match n with
  | 0 -> 1
  | 1 -> x
  | a ->
    let b = pow x (a / 2) in
    b * b * (if a mod 2 = 0 then 1 else a)

let rec mapi2 f tc g lst = (* mapi ki vzame dve funkciji in tocko*)
  let rec aux f tc g lst index =
    match lst with
    | [] -> []
    | x :: xs -> g (f tc index) x :: aux f tc g xs (index + 1)
in aux f tc g lst 0

let vrednost (poly: int list) n =
  List.fold_left ( + ) 0 (mapi2 pow n ( * ) poly)

(*
  3.5 Odvajanje
*)

let odvod = function
  | [] -> []
  | x :: xs -> xs

(*
  3.6 Lep Izpis
*)

let hd (a: int list): int =
  match a with
  | [] -> 0
  | x :: xs -> x

let tl = function
  | [] -> []
  | x :: xs -> xs

(* Ocaml threads: https://discuss.ocaml.org/t/should-we-have-a-string-rev-in-stdlib/9187/4 *)
let rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))

let izpis poly = 
  let rec aux lst index acc =
    if index = (List.length poly) then rev acc
    else
    if (index < List.length poly - 1) then
    aux (tl lst) (index + 1) (acc ^ Int.to_string index ^ "^x " ^ Int.to_string (hd lst) ^ " + ")
    else aux (tl lst) (index + 1) (acc ^ Int.to_string index ^ "^x " ^ Int.to_string (hd lst))
  in aux poly 0 ""

(*
  4. Samodejno Odvajanje
*)

type odvedljiva = (float -> float) * (float -> float)

let sinus : odvedljiva = (sin, cos)
let kosinus : odvedljiva = (cos, (fun x -> -. sin x))
let eksp : odvedljiva = (exp, exp)
let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
  (* pozorni bodite, da anonimni funkciji v paru date med oklepaje *)
  fun (f, f') (g, g') -> ((fun x -> f x +. g x), (fun x -> f' x +. g' x))

(*
  4.1 Vrednost Odvoda
*)

let vrednost (f : odvedljiva) (tc: float) : float =  fst(f) tc

let odvod (f : odvedljiva) (tc: float) : float = snd(f) tc

(*
  4.2 Osnovne Funkcije
*)
let konstanta (c: float) : odvedljiva = ((fun x -> c) , fun x -> 0.)

let identiteta : odvedljiva = ((fun x -> x), fun x-> 1.)

(*
  4.3 Produkt in Kvocient
*)

let ( **. ) (f : odvedljiva) (g : odvedljiva) : odvedljiva = 
( (fun x -> (fst f x *. fst g x) ), fun x -> (fst f x *. snd g x +. fst g x *. snd f x)  )

let ( //. ) (f : odvedljiva) (g : odvedljiva) : odvedljiva =
((fun x -> (fst f x /. fst g x )), (fun x -> (((snd f x *. fst g x) -. (fst f x *. snd g x)) /. (fst g x *. fst g x) ) ) )

(*
  4.4 Kompozitum
*)

let ( @@. ) (f : odvedljiva) (g : odvedljiva) : odvedljiva = 
let g0 = fst g in
let f0 = fst f in
let g1 = snd g in
let f1 = snd f in
((fun x -> g0 (f0 x)), (fun x -> g1 (f0 x) *. f1 x))

(*
  5. Substitucijska Sifra
*)

(*
  5.1 Sifriranje
*)

let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A')

(* Po nesreci napisal vigenerja :)
let sifriraj (str: string) key = 
  let rec aux str key index1 index2 acc =
    if (String.length str) - 1 = index1 then acc else 
    if (String.length key) - 1 = index2 
    then aux str key (index1 + 1) (0) (acc ^ String.make 1 (crka ((((indeks str.[index1] - indeks key.[index2]) mod 26) + 26 ) mod 26 ) ) )
    else aux str key (index1 + 1) (index2 + 1) (acc ^ String.make 1 (crka ((((indeks str.[index1] - indeks key.[index2]) mod 26) + 26) mod 26 ) ) )
  in aux str key 0 0 ""
*)


let sifriraj str key =
  let rec aux str key index acc =
    if index = (String.length str) then acc 
    else
    if (indeks str.[index] < 0 || indeks str.[index] > 25) then aux str key (index + 1) (acc ^ (String.make 1 str.[index]))
    else aux str key (index + 1) (acc ^ String.make 1 key.[indeks str.[index]])
  in aux str key 0 ""

(*
  5.2 Inverzni Kljuc
*)

let rec inverz key = 
  let abeceda = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let rec aux key index acc =
    if index = (String.length key) then acc
    else
    aux key (index + 1) (acc ^ String.make 1 (abeceda.[String.index_from key 0 abeceda.[index]]))
  in aux key 0 ""

(*
  5.3 Ugibanje Kljuca
*)

let besede =
  "the of to and a in is it you that he was for on are with as i his they be \
   at one have this from or had by word but what some we can out other were \
   all there when up use your how said an each she which do their time if will \
   way about many then them write would like so these her long make thing see \
   him two has look more day could go come did number sound no most people my \
   over know water than call first who may down side been now find any new \
   work part take get place made live where after back little only round man \
   year came show every good me give our under name very through just form \
   sentence great think say help low line differ turn cause much mean before \
   move right boy old too same tell does set three want air well also play \
   small end put home read hand port large spell add even land here must big \
   high such follow act why ask men change went light kind off need house \
   picture try us again animal point mother world near build self earth father \
   head stand own page should country found answer school grow study still \
   learn plant cover food sun four between state keep eye never last let \
   thought city tree cross farm hard start might story saw far sea draw left \
   late run don't while press close night real life few north open seem \
   together next white children begin got walk example ease paper group always \
   music those both mark often letter until mile river car feet care second \
   book carry took science eat room friend began idea fish mountain stop once \
   base hear horse cut sure watch color face wood main enough plain girl usual \
   young ready above ever red list though feel talk bird soon body dog family \
   direct pose leave song measure door product black short numeral class wind \
   question happen complete ship area half rock order fire south problem piece \
   told knew pass since top whole king space heard best hour better true . \
   during hundred five remember step early hold west ground interest reach \
   fast verb sing listen six table travel less morning ten simple several \
   vowel toward war lay against pattern slow center love person money serve \
   appear road map rain rule govern pull cold notice voice unit power town \
   fine certain fly fall lead cry dark machine note wait plan figure star box \
   noun field rest correct able pound done beauty drive stoDo contain front \
   teach week final gave green oh quick develop ocean warm free minute strong \
   special mind behind clear tail produce fact street inch multiply nothing \
   course stay wheel full force blue object decide surface deep moon island \
   foot system busy test record boat common gold possible plane stead dry \
   wonder laugh thousand ago ran check game shape equate hot miss brought heat \
   snow tire bring yes distant fill east paint language among grand ball yet \
   wave drop heart am present heavy dance engine position arm wide sail \
   material size vary settle speak weight general ice matter circle pair \
   include divide syllable felt perhaps pick sudden count square reason length \
   represent art subject region energy hunt probable bed brother egg ride cell \
   believe fraction forest sit race window store summer train sleep prove lone \
   leg exercise wall catch mount wish sky board joy winter sat written wild \
   instrument kept glass grass cow job edge sign visit past soft fun bright \
   gas weather month million bear finish happy hope flower clothe strange gone \
   jump baby eight village meet root buy raise solve metal whether push seven \
   paragraph third shall held hair describe cook floor either result burn hill \
   safe cat century consider type law bit coast copy phrase silent tall sand \
   soil roll temperature finger industry value fight lie beat excite natural \
   view sense ear else quite broke case middle kill son lake moment scale loud \
   spring observe child straight consonant nation dictionary milk speed method \
   organ pay age section dress cloud surprise quiet stone tiny climb cool \
   design poor lot experiment bottom key iron single stick flat twenty skin \
   smile crease hole trade melody trip office receive row mouth exact symbol \
   die least trouble shout except wrote seed tone join suggest clean break \
   lady yard rise bad blow oil blood touch grew cent mix team wire cost lost \
   brown wear garden equal sent choose fell fit flow fair bank collect save \
   control decimal gentle woman captain practice separate difficult doctor \
   please protect noon whose locate ring character insect caught period \
   indicate radio spoke atom human history effect electric expect crop modern \
   element hit student corner party supply bone rail imagine provide agree \
   thus capital won't chair danger fruit rich thick soldier process operate \
   guess necessary sharp wing create neighbor wash bat rather crowd corn \
   compare poem string bell depend meat rub tube famous dollar stream fear \
   sight thin triangle planet hurry chief colony clock mine tie enter major \
   fresh search send yellow gun allow print dead spot desert suit current lift \
   rose continue block chart hat sell success company subtract event \
   particular deal swim term opposite wife shoe shoulder spread arrange camp \
   invent cotton born determine quart nine truck noise level chance gather \
   shop stretch throw shine property column molecule select wrong gray repeat \
   require broad prepare salt nose plural anger claim continent oxygen sugar \
   death pretty skill women season solution magnet silver thank branch match \
   suffix especially fig afraid huge sister steel discuss forward similar \
   guide experience score apple bought led pitch coat mass card band rope slip \
   win dream evening condition feed tool total basic smell valley nor double \
   seat arrive master track parent shore division sheet substance favor \
   connect post spend chord fat glad original share station dad bread charge \
   proper bar offer segment slave duck instant market degree populate chick \
   dear enemy reply drink occur support speech nature range steam motion path \
   liquid log meant quotient teeth shell neck"

let slovar = List.map String.uppercase_ascii (String.split_on_char " ".[0] besede)

(*
  5.4 Razsirjanje Kljuca s Crko
*)

let dodaj_zamenjavo str (a, b) =
  let rec aux str index (a, b) acc =
    if index = String.length str then Some acc
    else
    if index != indeks a then aux str (index + 1) (a, b) (acc ^ String.make 1 str.[index])
    else
    if str.[index] = "_".[0] then aux str (index + 1) (a, b) (acc ^ String.make 1 b)
    else
    if str.[index] = b then aux str (index + 1) (a, b) (acc ^ String.make 1 b)
    else None
     
  in aux str 0 (a, b) ""

(*
  5.5 Razsirjanje Kljuca z Besedo
*)

let dodaj_zamenjave str (word, key) = 
  let rec aux str (word, key) index =
    if index = (String.length word) then Some str
    else
    match dodaj_zamenjavo str (word.[index], key.[index]) with
    | Some n -> aux n (word, key) (index + 1)
    | None -> None
  in aux str (word, key) 0

(*
  5.6 Vse Mozne Razsiritve
*)

let mozne_razsiritve str key slovar =
  let rec aux key slovar index acc =
  match slovar with
  | [] -> acc
  | x :: xs -> 
    if String.length x != String.length key then aux key xs (index + 1) acc
    else 
    match dodaj_zamenjave str (key, x) with
    | Some n -> aux key xs (index + 1) (acc @ ([n , x]))
    | None -> aux key xs (index + 1) acc
  in aux key slovar 0 []

(*
  5.7 Odsifriranje
*)


(* Ideja, vzamem prvo besedo in delam breath first search 
na njen dokler ne koncam ali se ne zalomi *)
(*
  
*)
let odsifriraj cyph = 
  let a = String.split_on_char ' ' cyph in
  let rec aux str sol carry =
  match carry with
  | [] -> sifriraj cyph (inverz sol)
  | key :: keys -> 
    match mozne_razsiritve str key slovar with
    | [] -> aux str sol carry (* Ne vem kaj vrnit; infinite loop *)
    | x :: xs ->  aux (fst x) (sol ^ snd x) keys
  in aux (String.make 26 '_') "" a