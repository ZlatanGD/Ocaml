(*Exercice 1 Factorielle*)
(*Q1*)
let rec fact (n: int) : int =
  if n = 0 then 1
  else n * (fact (n-1));;

let _ = assert((fact 5) = 120);;
(*Exercice 2*)
(*Q1*)
let rec sum_ (n:int) : int =
  if n = 0 then 0
  else n + (sum_ (n-1));;

let _ = assert((sum_ 5) = 5+4+3+2+1);;

(*Q2*)
let rec sum_n (n:int) : int = 
  if n = 0 then 0
  else if n < 0 then raise(Invalid_argument "sum_n: Valeur négative")
  else n + sum_n (n+1);;

(*Q3*)
let rec sum_p (n:int) : int =
  if n = 0 then 0
  else 2*n+(sum_p (n-1));;

let _ = assert((sum_p 4) = 8+6+4+2);;

let rec sum_f (f:int -> int)(n:int) : int =
  if n = 0 then f 0
  else (f n) + (sum_f f (n-1));;

let rec sum_p2 (n:int) : int =
  (sum_f (fun x -> 2*x) n);;

let _ = assert((sum_p2 4) = 8+6+4+2);;

(*Exercice 3*)
(*Q1*)
let rec u (n:int) : int =
  if n = 0 then 42
  else 3*(u (n-1)) + 4;;

let _ = assert((u 0) = 42);;
let _ = assert((u 1) = 3*42 + 4);;

(*Q2*)
let rec sum_u (n:int) : int =
  if n = 0 then 42
  else u n + sum_u (n-1);;

let _ = assert((sum_u 1) = 42 + 3*42+4);;

(*Q3*)
let sum_u2 (n:int) : int =
  let rec loop(m : int)(t:int) = 
    if m = 0 then t
    else t + (loop (m-1) (3*t+4))
  in (loop n 42);;

(*Exercice 4*)
(*Q1*)
let rec sum_inter(a:int)(b:int) : int = 
  if a = b then b
  else a + sum_inter (a + 1) b;;

let _ = assert((sum_inter 2 4) = 2+3+4);;

(*Q2*)
let rec sum1_inter (k:int)(a:int)(b:int) : int =
  if a = b then k+b
  else if k>b then 0
  else (k+a)+sum1_inter(k)(a+1)(b);;

let _ = assert((sum1_inter 1 2 4) = 12);;

(* Q3 *) 
let sum2_inter (a:int) (b:int) : int =
  if (a > b) then 0
  else let rec aux (temp:int) = 
         if temp = b+1 then 0
         else (sum1_inter temp a b) + (aux (temp + 1))
    in aux a;;

    





  
