  (*GOJAK Zlatan 3801873*)
  (*Travaux sur machine*)
  (*Exercice 5*)
(*Q1*)
let rec sigmerge (xs: 'a list) (ys: 'a list) : 'a list =
  match (xs, ys) with
  | [], _ -> ys
  | _, [] -> xs
  | x :: xss, y :: yss -> if x <= y then x :: y :: (sigmerge xss yss)
                          else y :: x :: (sigmerge xss yss)

let _ = assert((sigmerge [1;3;5;7] [2;2;4;5;9]) = [1;2;2;3;4;5;5;7;9])

(*let rec split (xs:'a list) : ('a list) * ('a list) =
  match xs with
  | [] -> []
  | x :: [] -> x *)

  (*Exercice 6*)
(* Q1 *)
let merge (cmp: 'a -> 'a -> bool) (xs:'a list) (ys:'a list) : 'a list =
  match (xs,ys) with
  | [], _ -> ys
  | _, [] -> xs
  | x :: xss, y :: yss -> if (cmp x y) then x :: y :: (sigmerge xss yss)
  else y :: x :: (sigmerge xss yss)
 
  let _ = assert((merge (fun x y -> x <= y) [2;5] [3;1]) = [2; 3; 1; 5])
  let _ = assert((merge (fun x y -> x >= y) [2;5] [3;1]) = [3; 2; 5; 1])