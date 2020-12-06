(* GOJAK Zlatan 3801873 *)

(* Exercice 1 *)
(*Q1*)
let len_comp_3 (xs: 'a list) : int =
  match xs with 
  | [] -> -1
  | x :: [] -> -1
  | x :: y :: [] -> -1
  | x :: y :: z :: []-> 0
  | _ -> 1

let _ = assert((len_comp_3([1])) = -1)
let _ = assert((len_comp_3([])) = -1)
let _ = assert((len_comp_3([1;2;3])) = 0)
let _ = assert((len_comp_3([1;2;3;4])) = 1)

(*Q2*)
let swap_hd_snd (xs:'a list) : 'a list =
  match xs with
  | [] -> [] 
  | x :: [] -> xs
  | x :: y :: ys -> y :: x :: ys 

  let _ = assert((swap_hd_snd([1;2;3;4])) = [2;1;3;4])
  let _ = assert((swap_hd_snd([1;2;3])) = [2;1;3])
  let _ = assert((swap_hd_snd([1;2])) = [2;1])
  let _ = assert((swap_hd_snd([1])) = [1])

(*Q3*)
let swap_hd_fst(xs:('a*'b) list) : ('a*'b) list = 
  match xs with
  | [] -> []
  | (a,b) :: ys -> (b,a) :: ys

let _ = assert((swap_hd_fst([(1,2); (2,3); (4,5)])) = [(2,1); (2,3); (4,5)])
let _ = assert((swap_hd_fst([])) = [])

(*Exercice 2*)
(*Q1*)
let rec repeat (n:int) (x: 'a) : 'a list =
  if n = 0 then []
  else if n < 0 then []
  else x :: (repeat (n-1) x)

let _ = assert((repeat 2 true) = [true; true])
let _ = assert((repeat 0 "Hello") = [])
let _ = assert((repeat (-42) "test") = [])

  (*Q2*)
  let rec range_i (i:int) (j:int) : (int list) =
    if i > j then []
    else i :: (range_i (i+1) j)
                  

let _ = assert((range_i 2 7) = [2; 3; 4; 5; 6; 7])

(*Exercice 3*)
(*Q1*)
let rec intercale1 (z:'a) (xs:'a list) : 'a list =
  match xs with
  | [] -> []
  | x :: [] -> xs
  | x :: ys -> x :: z :: (intercale1 z ys)

let _ = assert((intercale1 1 []) = [])
let _ = assert((intercale1 0 [5]) = [5])
let _ = assert((intercale1 0 [1;2;3]) = [1;0;2;0;3])

  (*Q2*)
let rec begaie (xs:'a list) : ('a list) =
  match xs with 
  | [] -> []
  | x :: ys -> x :: x :: (begaie ys) 

let _ = assert((begaie [1;2;3]) = [1;1;2;2;3;3])

(*Exercice 4*)
(*Q1*)
let rec inverse_i (xs:float list) : float list =
  match xs with
  | [] -> []
  | x :: ys -> float_of_int 1 /. x :: (inverse_i ys)

let _ = assert((inverse_i [1.0;2.0;3.0]) = [float_of_int 1/. float_of_int 1; float_of_int 1/. float_of_int 2 ; float_of_int 1 /. float_of_int 3])

(*Q2*)
let rec list_interval (ns:int list) : int list =
  match ns with
  | [] -> []
  | x :: xs -> if x >= (-10) && x <= 10 then x :: (list_interval xs)
               else list_interval xs

let _ = assert((list_interval [1;2;60;4;(-11);(-10);10]) = [1;2;4;(-10);10])

(*Q3*)
let rec parenthese (xs:string list) : string =
  match xs with
  | [] -> ""
  | x :: ys -> "("^x^")"^(parenthese ys)

let _ = assert((parenthese ["do";"re";"mi"]) = "(do)(re)(mi)")
