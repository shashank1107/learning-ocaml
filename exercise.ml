(* === *)
(* Solving exercises from https://ocaml.org/problems *)
(* === *)


(* Flatten a list *)
type 'a node =
  | One of 'a 
  | Many of 'a node list
;;

let flatten list = 
  let rec aux acc = function
  | [] -> acc
  | One x :: t -> aux (x :: acc) t
  | Many x :: t -> aux (aux acc x) t 
in 
aux [] (List.rev list)
;;


(* Eliminate Duplicates  *)
let duplicates list =
  let rec aux acc = function
  | [] -> []
  | [x] -> x :: acc
  | a :: (b :: _ as t) -> if a=b then aux acc t else aux (a :: acc) t
in
aux [] (List.rev list)
;;


(* Pack Consecutive Duplicates  *)
let pack_duplicate list = 
  let rec aux acc res = function
  | [] -> res
  | [x] -> (x::acc) :: res
  | h :: (x :: _ as t) -> if h=x then aux (h :: acc) res t else aux [] ((h :: acc) :: res) t
in
aux [] [] (List.rev list)
;;


(* Decode a Run-Length Encoded List  *)
type 'a node =
  | One of 'a
  | Many of (int * 'a)
;;

let decode list = 
  let rec add_ch ch count = if count <= 0 then [] else ch :: add_ch ch (count-1) 
in
let rec aux acc = function
| [] -> acc
| h :: t -> match h with 
  | One x -> aux (x :: acc) t
  | Many (count, x) -> aux ((add_ch x count) @ acc) t
in aux [] (List.rev list)
;;


(* Duplicate the Elements of a List *)
let duplicate list = 
  let rec aux acc = function
  | [] -> acc
  | h :: t -> aux (h :: (h :: acc)) t
in 
aux [] (List.rev list)
;;


(* Replicate the Elements of a List a Given Number of Times  *)
let replicate list n = 
  let rec replicate_ch ch cnt acc = if cnt<=0 then acc else replicate_ch ch (cnt-1) (ch::acc)
in
let rec aux acc n = function
| [] -> acc
| h :: t -> aux (replicate_ch h n acc) n t
in
aux [] n (List.rev list)
;;


(* Drop Every N'th Element From a List  *)
let drop list n = 
  let rec aux acc cnt = function
  | [] -> List.rev acc
  | h :: t -> if cnt=n then aux acc 1 t else aux (h :: acc) (cnt+1) t
in
aux [] 1 list
;;


(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let split list n = 
  let rec aux acc count = function
  | [] -> ((List.rev acc), [])
  | h :: t as l -> if count=0 then ((List.rev acc), l) else aux (h::acc) (count-1) t
in
aux [] n list
;;


(* Extract a Slice From a List  *)
let slice list i j = 
  let rec aux acc count = function
  | [] -> List.rev acc
  | h :: t -> if count < i then aux acc (count+1) t else if count > j then List.rev acc else aux (h :: acc) (count+1) t
in
aux [] 0 list
;;