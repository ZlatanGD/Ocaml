(* 3801873 Gojak Zlatan Groupe 4 *)

(*Exercice 5: Tri fusion*)
(*Q1*)
let rec merge (xs : 'a list)(ys: 'a list) : 'a list =
  match xs, ys with
  |[], _ -> ys
  | _, [] -> xs
  | x::xss, y::yss -> if(x = y) then x :: y :: (merge xss yss)
      else if (x < y) then x :: (merge xss ys)
      else y :: (merge xs yss);;

let _ = assert((merge [1;5] [3;6;2]) = [1;3;5;6;2]);;
let _ = assert((merge [0;2;4;8] [1;3;5;7]) = [0;1;2;3;4;5;7;8]);;
let _ = assert((merge [0;1;2;4] [2;3;4;5]) = [0;1;2;2;3;4;4;5])

(*Q2*)
let rec split (xs:'a list) : ('a list * 'a list) =
  match xs with 
  |[]->([],[])
  |x::[]->([x],[])
  |x::y::ys->
      let (a,b)=split ys in (x :: a, y :: b);; 

let _ = assert((split [1;2;3;4]) = ([1;3],[2;4]));;
let _ = assert((split [1;2;3]) = ([1;3],[2]));;


(*Q3*)
let rec merge_sort(xs:'a list) : 'a list =
  match xs with
  | [] -> []
  | x :: [] -> xs
  | _ -> let a,b = split(xs) in (merge (merge_sort a)(merge_sort b));;

let _ = assert((merge_sort [1;5;4;0;9]) = [0;1;4;5;9]);;

(*Exercice 6: D'autres ordres*)
(*Q1*)
let rec merge_gen (cmp:'a -> 'a -> bool) (xs:'a list) (ys:'a list) : 'a list =
  match xs, ys with
  | [], [] -> []
  | [], y::yss -> ys
  | x :: xss, [] -> xs
  | x :: xss, y :: yss -> if cmp x y then x :: (merge_gen cmp xss ys)
      else y :: (merge_gen cmp xs yss);;

let _ = assert((merge_gen (fun x y -> x >= y) [2;5] [3;1]) = [3;2;5;1]);;

(*Q2*)
let rec merge_sort_gen(cmp: 'a -> 'b -> bool)(xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | x :: [] -> xs
  | x :: xss -> let a,b = split(xs) in (merge_gen cmp (merge_sort_gen cmp a) (merge_sort_gen cmp b));;

  let _ = assert((merge_sort_gen (fun x y -> x > y) [1;3;2;4;9]) = [9;4;3;2;1]);;
  
(*Q3*)
  let sort (xs:(int * int) list): ((int * int) list) =
    let cmp_fun ((x1, x2): int * int) ((y1, y2): int * int): bool = (x1 + x2) < (y1 + y2) 
    in merge_sort_gen cmp_fun xs;;
  
let _ = assert((sort [(6,2);(1,2);(0,1)]) = [(0,1);(1,2);(6,2)]);;