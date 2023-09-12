(* Statically typed functional language *)

(* Interpreter binary - ocaml & compiler is ocamlopt. There is bytecode compiler ocamlc *)

(* It handles type inference using Hindley-Milner algorithm *)

let inc x = x + 1;
(* syntax: let {function_name} {*variables} = {function_body}  *)
(* Toplevel prints type in the form - int -> int i.e. type of all the input variables and last is the return type *)

let sum a b = a + b;
(* Toplevel: int -> int -> int *)

(* a' => a_tmp in other languages *)

let rec fact n = if n=0 then 1 else n * fact (n-1);
(* declare `rec` keyword is function is recursive *)
(* Paranthesis is not required while calling function; unless argument is an expression *)

(* 
  (* unit type ?????  *)
  (* If a function does not return anything, it returns unit type. *)
*)

(* Partial functions: Above is the definition of how you can declare partial functions. Also, called function currying *)
let add x y = x+y;
let add_1= add 1;
add_1 1;


(* You can chain let ... in construct *)
let x = 10 in
let y = 20 in
x + y;
 

 (* Declare anonymous functions *)
 let my_lambda = fun x -> x * x;


 (* 
    (* Operators & Functions *)
    (* There are built in operators in Ocaml. However, there is little distinction between them. Operator can be called functions and overridden *)

    (* Ocaml refrain from type conversion like int to float; instead there are separate operators for floats *)
    (* Unary operators are an exception; they are polymorphic *)
  *)
  let (~/) x = 1.0 /. x ; 
  (* Example of unary reciprocal operator. It starts with `~` symbol *)

  (* === Data Structures === *)

  (* List - one type; separated by semicolon; declared in square brackets; *)
  let lst = [1;2;3];
  (* List functions *)
  List.nth a 1;
  List.map (fun x -> x * 2) [1;2;3;];
  (* Append element to the beginning of list with "::" constructor called "cons" operator *)
  1 :: [2;3] 
  
  (* Tuples - any types; optionally enclosed in parentheses; separated by commas *)
  let tup = (1,2,"Shashank");
  let tup = 1, 2, "Shashank";

  (* Named tuples are called records *)
  let record = {num: float; denom: int};

  (* Arrays - one type; enclosed in [|  |] *) 
  (* Arrays are mutable data types *)
  let arr = [| 1;2;3; |]
  (* Can directly access array items with an index *)
  arr.(0);

  (* Strings and Characters *)
  let a = "shashank";
  (* Encolsed in double quotes *)
  let a = 's';
  (* Enclosed in single quotes *)
  (* Strings are not arrays of chars and hence should not be mixed together in an expression *)

  (* Records - User defined data type *)
  type lang = Ocaml | ML | Lisp;
  (* These values are called constructors *)
  type lang = Ocaml | ML | Lisp of float;
  (* Constructor can be of a particular type *)
  type point = Point of float * float;
  let p = Point (2., 3.);

  (* Another imp example of variants is built-in 'a option type *)
  type 'a option = Some of int | None; 
  (* 'a will either have some value of type int or absense of value i.e. None *)

   (* Most common usage of variant types is to describe recursive data structures like tree *)
   type 'a btree = Empty | Node of 'a * 'a btree * 'a btree;
   (* Add apostrophe as prefix to a var name means it's of arbitrary type *)


  (* === Pattern Matching === *)
  (* One of the most important feature of Ocaml; Very similar to switch case constructs in other languages but more powerful *)
  (* Can match an argument against: Exact Value; Predicate; Type constructor *)
  let is_zero y = match y with 
  | 0 -> true
  | _ -> false
  ;
  let abs x = 
    match x with 
    | x when x < 0 -> -x
    | _ -> x
  ;
  type laptop = Macbook of String | Lenovo of String;
  let is_mac x = 
    match x with 
    | Macbook x -> x ^ " is macbook"
    | Lenovo x -> x ^ " is lenovo"
  ;
  is_mac (Macbook "pro");


  (* === IMPERITIV FEATURES ===  ???? *)
  
  
  (* === EXCEPTIONS === *)
  (* Can define exception with `exception` keyword *)
  exception EmptyList;
  let find arr k = match arr with
  | [] -> raise EmptyList
  | h::t -> List.assoc k arr;
  find [(1, "one"); (2, "two")] 3;
(* above expression raises "Exception: Not_found."  *)

(* Exceptions can be caught with "try...with" construct. You can also pattern match on "with" *)
let find_pos arr k = 
  try
    find arr k
  with
  | EmptyList -> "List is empty"
  | Not_found -> "Cannot find key in the array";

  (* You can also write like below *)
  let find_pos arr k = 
    match find arr k with
    | exception EmptyList -> None
    | None | exception Not_found -> None
    | Some _ as v -> v;


  (* Lazy expressions *)
  (* You can delay the evaluation of an expression to when you actually need it  *)
  let add a b = lazy (print_endline "Lazy evaluation"; a+b);
  add 2 3; 
  (* This will return <lazy>  *)
  Lazy.force (add 2 3);
  (* This will actually evaluate the expression *)
