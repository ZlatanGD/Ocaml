let rec fact(a:int) : int =
  if a = 0 then 1
  else if a < 0 then raise(Invalid_argument "fact: Valeur négative")
  else a*fact(a-1);;

let _ = assert((fact 5) = 120);;

let rec sum (a:int) : int =
  if a = 0 then 0
  else if a < 0 then raise(Invalid_argument "sum: Valeur négative")
  else a + sum(a-1);;

let _ = assert((sum 5) = 15);;

let rec sum_p(a:int) : int =
  if a = 1 then 2
  else if a < 1 then raise(Invalid_argument "sum_p: Valeur non accepté")
  else a*2 + sum_p(a-1);;

let _ = assert((sum_p 4) == 20);;

let rec sum_f(f:int -> int)(n:int) : int =
  if n = 0 then f 0
  else (f n) + (sum_f f (n-1));;

let rec u (n:int) : int =
  if n = 0 then 42
  else 3*(u (n-1))+4;;
(*L'hypothese est que la valeur de n doit etre positive!*)

let _ = assert((u 0) = 42);;
let _ = assert((u 2) = 394);;

let u1 (n:int) : int =
  let rec aux(temp:int) : int =
    if temp = 0 then 42
    else 3*(aux (temp-1)) + 4
  in aux n;;

let _ = assert((u1 0) = 42);;
let _ = assert((u1 2) = 394);;

let rec sum_u(n:int) : int =
  if n = 0 then 0
  else (u n) + (sum_u (n-1));;

let sum_u2(n:int) : int =
  let rec loop(n:int)(t:int) : int =
    if (n > 0) then t + (loop (n-1) (3*t+4))
    else t
    in (loop n 42);;

let rec sum_inter(a:int)(b:int) : int =
  if a = b then a
  else a + sum_inter(a + 1) b;;

let _ = assert((sum_inter 2 5) = 14);;

let rec sum_inter1(k:int)(a:int)(b:int) : int =
  if a = b then k + b
  else k+a + sum_inter1 k (a+1) b;;

let _ = assert((sum_inter1 1 2 3) = 7);;

let rec sum_inter2 (a:int)(b:int) : int =
  if (a < b) then 0
  else let rec aux(temp:int) : int = 
  if temp = b + 1 then 0
  else (sum_inter1 temp a b) + aux (temp + 1)
  in aux a;;

