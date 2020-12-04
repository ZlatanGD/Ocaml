(*GOJAK Zlatan 3801873*)

    (* == TD2 *)

(* == Exercice I: Factorielle *)

let rec fact(a:int) : (int)=
if a = 0 then 1
else if a<0 then raise(Invalid_argument "fact: Valeur négative")
else a*fact(a-1);;

let _ = assert((fact(5)) == 120);;


(* == Exercice II: Sommes *)

let rec sum_n (n:int) : (int) =
if n<0 then raise(Invalid_argument "sum_n: Valeur négative")
else if n = 0 then 0
else n+sum_n(n-1);;

let _ = assert((sum_n(5)) == 15);;
let _ = assert((sum_n(4)) == 10);;


let rec sum_p (n:int) : (int) = 
if n = 1 then 2
else (n*2)+sum_p(n-1);;

let _ = assert((sum_p(4)) == 20);;
let _ = assert((sum_p(3)) == 12);;

(*let rec sum_f(f:int -> int) (n:int) : (int)=
if n = 0 then f 0
else f n +sum_f(f)(n-1);;

let _ = assert((sum_f(sum_p) (4)) == 20);;*)

(* == Exercice III: Termes d'une suite *)

let rec u (n:int) : (int) =
if n = 0 then 42
else 3*u(n-1)+4;;

let _ = assert((u(0)) == 42);;
let _ = assert((u(1)) == 130);;
let _ = assert((u(2)) == 394);;

let rec sum_u(n:int) : (int) =
if n = 0 then 42
else u n +sum_u(n-1);;

let _ = assert((sum_u(0)) == 42);;
let _ = assert((sum_u(1)) == 172);;
let _ = assert((sum_u(2)) == 566);;

(* == Exercice IV: Recurrence sur un intervalle*)

let rec sum_inter(a:int)(b:int) : (int) =
if a = b then b
else a+sum_inter(a+1)(b);;

let _ = assert((sum_inter(1)(3)) == 6);;
let _ = assert((sum_inter(1)(4)) == 10);;
let _ = assert((sum_inter(6)(12)) == 63);;

let rec sum1_inter(k:int)(a:int)(b:int) : (int) =
if a = b then k+b
else k+a+sum1_inter(k)(a+1)(b);;

let _ = assert((sum1_inter(2)(1)(3)) == 12);;
let _ = assert((sum1_inter(10)(6)(8)) == 51);;

(*let sum2_inter(a:int)(b:int) : (int) = 
for i = a to b do sum1_inter((a+i)(a)(b)) done;;

let _ = assert((sum2_inter(2)(4)) == 54);;*)


















