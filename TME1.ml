(*Exercice IV: Addition binaire*)
(*GOJAK Zlatan 3801873*)

(* == Question 1 *)

type bit = bool;;
type duet = bit * bit;;
type quartet = bit * bit * bit * bit;;

(* == Question 2 *)

let xor (a:bit)(b:bit):(bit) = (a && not b) || (not a && b);;

let _ = assert((xor false false)=false);;
let _ = assert((xor false true)=true);;
let _ = assert((xor true false)=true);;
let _ = assert((xor true true)=false);;

(* == Question 3 *)
  
let half_adder (a: bit)(b:bit):(duet) = 
  (xor a b, a && b);; 

let _ = assert((half_adder false false = (false, false)));;
let _ = assert((half_adder false true = (true, false)));;
let _ = assert((half_adder true false = (true, false)));;
let _ = assert((half_adder true true = (false, true)));; 

(* == Question 4 *)

let adder(a:bit)(b:bit)(c:bit):(duet)=
  let (s1, r1) = half_adder a b in let (s2,r2) = half_adder s1 c in 
  (s2, r1 || r2);;

(* == Question 5 *)

let _ = assert((adder false false false)=(false, false));;
let _ = assert((adder true true true)=(true, true));;
let _ = assert((adder false true false)=(true, false));;
let _ = assert((adder true false true)=(false, true));;
let _ = assert((adder true false false)=(true, false));;
let _ = assert((adder false false true)=(true, false));;

(* == Question 6 *)

let duet_adder(a:duet)(b:duet)(c:bit) : (duet*bit) =
  let(a1, a2) = a in  let(b1, b2)=b in
  let(s1, r1)=adder a2 b2 c in let(s2, r2)=adder a1 b1 r1 in 
  ((s2, s1),r1);;

(* == Question 7 *)

let quartet_adder(a:quartet) (b:quartet) (c:bit) :(quartet * bit) =
  let(a0, a1, a2, a3) = a in let (b0, b1, b2, b3) = b in 
  let ((s2,s1),r1) = (duet_adder (a2, a3) (b2, b3) c) in
  let ((s4, s3),r2) = (duet_adder(a0, a1)(b0, b1) r1) in 
  ((s4, s3, s2, s1),r2);;


(* == Question 8 *)
  
let to_quartet (n4: int): (quartet) =
let r0 = n4 mod 2
and r1 = n4/2 mod 2
and r2 = n4/4 mod 2
and r3 = n4/4 mod 2 in
(r3 = 1, r2 = 1, r1 = 1, r0 = 1)

(* == Question 9 *)

let () = assert (quartet_adder (to_quartet 8) (to_quartet 7) false = ((to_quartet 15), false))
(*let () = assert (quartet_adder (to_quartet 8) (to_quartet 8) true = ((to_quartet 1), true))*)


  


