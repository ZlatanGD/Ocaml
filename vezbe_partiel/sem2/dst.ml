(*Exercice 2: Suites récursives*)
(*Q1*)
let rec u (n:int) : int = 
  if  n = 0 then 5
  else n*n + (u (n-1));;

  let _ = assert((u 0) = 5);;
  let _ = assert((u 1) = 6);;

(*Q2*)
let rec u2 (n:int) : int =
  if  n = 0 then 5
  else if n < 0 then raise(Invalid_argument "u2: Valeur négative")
  else n*n + (u (n-1));;

(*Q3*)
let rec u2t (n:int)(temp:int) : int =
  if n = 0 then temp
  else (u2t (n-1) (n*n + temp));;

let _ = assert((u2t 0 5) = 5);;
let _ = assert((u2t 1 5) = 6);;
let _ = assert((u2t 2 5) = 10);;

(*Exercice 3: Listes sans doublons*)
(*Q1*)
let eq_list xs ys =
  (List.length xs) = (List.length ys)
  && (List.for_all (fun y -> List.mem y xs) ys)
  && (List.for_all (fun x -> List.mem x ys) xs)

let rec add_item(n:'a)(xs: 'a list) : 'a list =
  match xs with
  | [] -> n :: xs
  | x :: xss -> if (x=n) then xs
  else x :: (add_item n xss);;

  let _ = assert((add_item 0 []) = [0]);;
  let _ = assert((add_item 0 [0;1;2]) = [0;1;2]);;
  let _ = assert((add_item 0 [2;1;2]) = [2;1;2;0]);;

let rec add_list(xs : 'a list)(ys: 'a list) : 'a list =
  match xs, ys with
  | [],[]-> []
  | _,[] -> xs
  | [],_ -> ys
  | x :: xss, y :: yss-> add_list xss (add_item x ys);;
  
let _ = assert (eq_list (add_list [] [1;2;3]) [1;2;3]);;
let _ = assert (eq_list (add_list [1;2;3] [])  [1;2;3]);;
let _ = assert (eq_list (add_list [1;2] [1;2;3]) [1;2;3]);;
let _ = assert (eq_list (add_list [1;2] [3;4;5]) [1;2;3;4;5]);;

let rec add_list_list (xss:('a list) list) : 'a list =
  match xss with
  | [] -> []
  | x :: xs -> add_list x (add_list_list xs);;

let _ = assert (eq_list (add_list_list []) [])
let _ = assert (eq_list (add_list_list [[1;2;3]])  [1;2;3])
let _ = assert (eq_list (add_list_list [[1;2;3];[4;5]]) [1;2;3;4;5])
let _ = assert (eq_list (add_list_list [[1;2;3]; [4;3]]) [1;2;3;4])

type cat = (string * float) list;;

    
let tarifs1 =
  [("lait", 2.35);
   ("beurre", 5.30);
   ("cornichons", 3.80);
   ("concombre", 7.21);
   ("raviolis", 5.75);
   ("poisson", 17.4);
   ("carottes", 4.95)]

   let tarifs2 =
    [("lait", 2.55);
     ("beurre", 5.30);
     ("cornichons", 3.80);
     ("concombre", 7.21);
     ("raviolis", 5.45);
     ("poisson", 18.4);
     ("carottes", 4.95)]

let rec prix_list(xs:string list)(tarifs : cat) : float =
  match xs with
  | [] -> 0.0
  | x :: xss -> match tarifs with
                | [] -> 0.0
                | (a,b) :: tarifss -> if (x = a) then b+.(prix_list xss tarifs)
                else prix_list xs tarifss;;


let _ = assert ((prix_list [] tarifs1) = 0.0);;
let _ = assert ((prix_list ["beurre"; "poisson"] tarifs1) = 22.7);;

let rec min_prix_list(xs: string list)(tarifs1:cat)(tarifs2 : cat) : float =
  match xs with
  | [] -> 0.0
  | x :: xss -> match tarifs1, tarifs2 with
                | [],_ -> prix_list xs tarifs2
                | _, [] -> prix_list xs tarifs1
                | t1::tarifss1, t2::tarifss2 -> min(List.assoc x tarifs1)(List.assoc x tarifs2) +. (min_prix_list xss tarifs1 tarifs2);;

let _ = assert((min_prix_list ["beurre"; "poisson"] tarifs1 tarifs2) = 22.7);;

let rec split_list(xs: string list)(tarifs1: cat)(tarifs2: cat) : (string list*string list) = 
  match xs with
  | [] -> ([],[])
  | x :: xss -> match tarifs1, tarifs2 with
                | [],_ -> ([], xs)
                | _,[] -> (xs, [])
                | t1 :: tarifss1, t2 :: tarifss2 ->let (a,b)= split_list xss tarifs1 tarifs2 in
                                                   if (List.assoc x tarifs1 <= List.assoc x tarifs2) then ((x :: a),b)
                                                   else (a, (x::b))

              

let _ = assert ((split_list [] tarifs1 tarifs2) = ([],[]))
let _ = assert ((split_list ["lait";"poisson";"raviolis"] tarifs1 tarifs2) = (["lait";"poisson"],["raviolis"]))

(*Exercice 5: Schema d'iteration*)
let rec liste_diff(xs:float list)(m:float) : float list =
  List.map (fun x-> x -. m) xs;;

let _ = assert((liste_diff [1.0;2.0;3.0] 1.0) = [0.0;1.0;2.0]);;

let rec liste_inter(xs:float list)(m1: float)(m2:float) : float list =
  match xs with
  | [] -> []
  | x :: xss -> if ((x >= m1) && (x <= m2)) then x :: (liste_inter xss m1 m2)
                else liste_inter xss m1 m2;;

let _ = assert((liste_inter [1.0;2.0;3.0;4.0;5.0;6.0] 2.0 4.0) = [2.0;3.0;4.0]);;

let rec sum_square(xs: float list) : float =
  match xs with
  | [] -> 0.0
  | x :: xss -> x*.x +. (sum_square xss)

let _ = assert((sum_square [2.0;3.0;4.0]) = 29.0);;





