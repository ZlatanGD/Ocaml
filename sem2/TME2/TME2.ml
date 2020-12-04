(* == Travaux sur machines *)

(*== Exercice V: Nombres premiers*)

(*== Question 1 *)
let rec less_divider (i:int) (n:int) : int =
  if (n == i) then 0
  else if ((n mod i) = 0) then i
  else less_divider(i+1) n;;

let _ = assert((less_divider(2)(2)) == 0);;
let _ = assert((less_divider(2)(20)) == 2);;
let _ = assert((less_divider(1)(3)) == 1);;

(*== Question 2 *)

let prime (n:int) : bool = 
  if n=1 then false else less_divider 2 n =0;;

let _ = assert((prime 13) == true);;
let _ = assert((prime 14) == false);;

(*== Question 3 *)

let rec next_prime (n:int) : int=
  if (prime(n) == true) then n
  else next_prime(n+1);;


(*== Question 4 *)
let nth_prime (n:int):int= 
  let rec aux (a: int) (p: int) = 
    if a = 0 then p
    else aux (a-1) (next_prime (p+1))
  in aux n 2

let _ = assert((nth_prime 0) == 2);;
let _ = assert((nth_prime 4) == 11);;

(*== Exercice VI: Approximation de la racine carée*)

(*== Question 1*)

let f (x:float) (a:float): float =  (1. /. 2.) *. (x +. (a /. x));;

(*== Question 2*)

let sqrt_n (n: int) (a: float) (x0: float): float =
	let rec aux (b: int) (c: float) = 
		if b = 0 then c
		else aux (b-1) (f a c)
	in if (a < 0.) || (n < 0) then raise(Invalid_argument "Valeur négative")
	else aux n x0


(*== Question 3*)

let eq_eps (e: float) (x: float) (y: float): bool = (abs_float (x-.y)) < e

(*== Question 4*)

let sqrt_x (e:float) (a:float) (x0:float) : float =
  let rec aux (x: float) = 
    if eq_eps e x (f a x) then f a x
    else aux (f a x)
  in if e < 0. then raise (Invalid_argument "Valeur négative")
  else aux x0;;



