(* 3801873 Gojak Zlatan Groupe 4 *)

(*Exercice 1: Filtrages sur les listes*)
(*Q1*)
let len_comp_3 (xs:'a list) : int =
  match xs with
  | [] -> -1
  | x :: [] -> -1
  | x :: y :: [] -> -1
  | x :: y :: z :: [] -> 0
  | _ -> 1

  let _ = assert((len_comp_3 []) = -1);;
  let _ = assert((len_comp_3 [1;2]) = -1);;
  let _ = assert((len_comp_3 [1;2;3;4]) = 1);;
  let _ = assert((len_comp_3 [1;2;3]) = 0);;

(*Q2*)
let swap_hd_snd(xs:'a list) : 'a list =
  match xs with
  | x :: y :: xss -> y :: x :: xss
  | _ -> xs;;

let _ = assert((swap_hd_snd [1;2;3;4]) = [2;1;3;4]);;

(*Q3*)
let swap_hd_fst (xs:('a*'a) list) : ('a*'a) list =
  match xs with
  | [] -> []
  | (a,b) :: xss -> (b,a) :: xss

let _ = assert((swap_hd_fst [(1,2); (3,4); (5,5)]) = [(2,1); (3,4); (5,5)]);;

(*Exercice 2: Contructions de liste avec récursion sur entiers*)
(*Q1*)

let rec repeat(n:int)(x:'a) : 'a list =
  if n = 0 then []
  else x :: (repeat (n-1) x);;

let _ = assert((repeat 2 true) = [true;true])

(*Q2*)
let rec range_i(i:int)(j:int) : (int list) = 
  if i > j then []
  else i :: (range_i(i+1) j);;

let _ = assert((range_i 2 5) = [2;3;4;5])

(*Exercice 3: Manipulationde listes récursion sur listes*)

let rec intercale1 (z:'a)(xs:'a list) : 'a list =
  match xs with
  | [] -> []
  | x :: [] -> xs
  | x :: xss -> x :: z :: (intercale1 (z) xss);;

let _ = assert((intercale1 0 [1;2]) = [1;0;2]);;

let rec begaie (xs: 'a list) : ('a list) =
  match xs with
  | [] -> []
  | x :: xss -> x :: x :: (begaie xss);;

let _ = assert((begaie [1;2;3]) = [1;1;2;2;3;3]);;

(*Exercice 4: Application, filtrage, accumulation*)
(*Q1*)
let rec inverse_i (xs:int list) : float list =
  match xs with
  | [] -> []
  | x :: xss -> 1. /. float_of_int x :: (inverse_i xss);;

let _ = assert((inverse_i [1;2;3]) = [1./.1.;1./.2.;1./.3.]);;

(*Q2*)
let rec list_interval(ns: int list) : int list =
  match ns with
  | [] -> []
  | x :: xss -> if ((x >= (-10)) && (x <= 10)) then x :: (list_interval xss)
  else list_interval xss;;

let _ = assert((list_interval [-20;10;9;-3;-32]) = [10;9;-3]);;

(*Q3*)
let rec parenthese(xs:string list) : string =
  match xs with
  | [] -> ""
  | x :: xss -> "(" ^ x ^ ")" ^ (parenthese xss);;

let _ = assert((parenthese ["do";"re";"mi"]) = "(do)(re)(mi)");;
