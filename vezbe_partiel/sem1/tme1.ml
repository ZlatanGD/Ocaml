type bit = bool;;
type duet = bool * bool;;
type quartet = bool * bool * bool * bool;;

let xor (a:bit)(b:bit) : bit =
  if ((a = true) && (b = true)) then false
  else if ((a = false) && (b = false)) then false
  else true;;

let _ = assert((xor false false)=false);;
let _ = assert((xor false true)=true);;
let _ = assert((xor true false)=true);;
let _ = assert((xor true true)=false);;

let half_adder(a:bit)(b:bit) : (bit*bit) = 
  (xor a b, a && b);;

let _ = assert((half_adder false false = (false, false)));;
let _ = assert((half_adder false true = (true, false)));;
let _ = assert((half_adder true false = (true, false)));;
let _ = assert((half_adder true true = (false, true)));; 

let adder(a:bit)(b:bit)(c:bit) : (bit*bit) = 
  let (s1, r1) = half_adder a b in let (s2, r2) = half_adder s1 c in
  (s2, r1 || r2);;

let _ = assert((adder false false false)=(false, false));;
let _ = assert((adder true true true)=(true, true));;
let _ = assert((adder false true false)=(true, false));;
let _ = assert((adder true false true)=(false, true));;
let _ = assert((adder true false false)=(true, false));;
let _ = assert((adder false false true)=(true, false));;

let duet_adder ((a,b):duet) ((c,d):duet) (r:bit) :(duet * bit) =
  let (s1,r1) = (adder b d r)
  in let (s2,r2) = (adder a c r1)
  in ((s2,s1), r2);;

let quartet_adder ((a,b,c,d):quartet) ((e,f,g,h):quartet) (r:bit) :(quartet * bit) =
  let ((b3,b4),r1) = (duet_adder (c,d) (g,h) r)
  in let ((b1,b2),r2) = (duet_adder (a,b) (e,f) r1)
  in ((b1,b2,b3,b4),r2);;

  let to_quartet (i : int) : quartet =
    let r0 = i mod 2 in
    let r1 = i / 2 mod 2 in
    let r2 = i / 4 mod 2 in
    let r3 = i / 8 mod 2 in
    (r3 = 1, r2 = 1, r1 = 1, r0 = 1);;