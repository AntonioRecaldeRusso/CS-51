
open Core.Std ;;
open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry
 *    about expressionLibrary.ml
 * 3. Remember to test using the function "assert".
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
  match e with
  | Var -> true
  | Num _ -> false
  | Binop (_, x, y) -> contains_var x || contains_var y
  | Unop (_,y) -> contains_var y
;;

assert( contains_var (parse "x^2") = true );;
assert( contains_var (parse "2^x") = true );;
assert( contains_var (parse "2+3") = false);;
assert( contains_var (parse "((3+2)-(1*5)/(sin 3)*(ln(x)))") = true );;


(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Don't
 *            worry about handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
  | Var -> x
  | Num n -> n
  | Binop (op, y, z) ->
    (match op with
     | Add -> (+.) (evaluate y x) (evaluate z x)
     | Sub -> (-.) (evaluate y x) (evaluate z x)
     | Mul -> ( *. ) (evaluate y x) (evaluate z x)
     | Div -> (/.) (evaluate y x) (evaluate z x)
     | Pow -> ( ** ) (evaluate y x) (evaluate z x))
  | Unop (op, y) ->
    (match op with
     | Sin -> (sin) (evaluate y x)
     | Cos -> (cos) (evaluate y x)
     | Ln  -> (log10) (evaluate y x)
     | Neg -> (~-.) (evaluate y x))
;;

assert( evaluate (parse "x^4 + 3") 2.0 = 19.0 );;
assert( evaluate (parse "(2*x)+4") 5. = 14.);;
assert( evaluate (parse "(cos x)-(sin x)") 0. = 1.);;

(*>* Problem 2.3 *>*)

(* See writeup for instructions. We have pictures! *)
let rec derivative (e:expression) : expression =
    match e with
    | Num _ -> Num 0.
    | Var -> Num 1.
    | Unop (u,e1) ->
        (match u with
        | Sin -> Binop(Mul,Unop(Cos,e1),derivative e1)  
        | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
        | Ln -> Binop(Mul,Binop(Div,Num 1.,e1),derivative e1) 
        | Neg -> Unop(Neg,derivative e1))
    | Binop (b,e1,e2) ->
        match b with
        | Add -> Binop(Add,derivative e1,derivative e2)
        | Sub -> Binop(Sub,derivative e1,derivative e2)
        | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                        Binop(Mul,derivative e1,e2))
        | Div -> Binop(Div,
                       Binop(Sub,
                             Binop(Mul,derivative e1, e2),
                             Binop(Mul,e1,derivative e2)),
                       Binop(Pow, e2, Num 2.))
        | Pow ->
           if contains_var e2
           (* case: exponent has variable *)
           then Binop(Mul,
                      Binop(Pow,e1,e2),
                      Binop(Add,
                            Binop(Mul,derivative e2,Unop(Ln,e1)),
                            Binop(Div,Binop(Mul,derivative e1,e2),e1)))
           (* no variables in exponent *)
           else Binop(Mul,
                      Binop(Mul,e2,derivative e1),
                      Binop(Pow,e1,Binop(Sub,e2,Num 1.)))
;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
    print_string ("Checking expression: " ^ strs ^ "\n");
    let parsed = parse strs in (
        print_string "contains variable : ";
        print_string (string_of_bool (contains_var parsed));
        print_endline " ";
        print_string "Result of evaluation: ";
        print_float (evaluate parsed xval);
        print_endline " ";
        print_string "Result of derivative: ";
        print_endline " ";
        print_string (to_string (derivative parsed));
        print_endline " "
    )
;;


assert( to_string (derivative (parse "5")) = "0.");;
assert( to_string (derivative (parse "x")) = "1.");;
assert( to_string (derivative (parse "x^2")) = "((2.*1.)*(x^(2.-1.)))" );;
assert( to_string (derivative (parse "sin x")) = "((cos(x))*1.)");;
assert( to_string (derivative (parse "cos x")) = "((~((sin(x))))*1.)" );;
assert( to_string (derivative (parse "ln x")) = "((1./x)*1.)");;
assert( to_string (derivative (parse "x*x")) = "((x*1.)+(1.*x))" );;
assert( to_string (derivative (parse "x/x")) = "(((1.*x)-(x*1.))/(x^2.))");;
assert( to_string (derivative (parse "x^x")) = "((x^x)*((1.*(ln(x)))+" ^
						"((1.*x)/x)))");;
assert( to_string (derivative (parse "x+x")) = "(1.+1.)");;
assert( to_string (derivative (parse "x-x")) = "(1.-1.)");;



(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
  let g1 = evaluate (Binop(Sub,Num g,Binop(Div,e,derivative e))) g in
  if lim = 0
  then  (if (evaluate e g1) < epsilon then Some g1 else None) 
  else find_zero e g1 epsilon (lim - 1) 
;;

(* This function was made for the purpose of comparing float option values *)
let test_float (v1:float option) (v2:float option) (epsilon:float) =
  match v1, v2 with
  | Some x, Some y -> if Float.abs(x -. y) < epsilon then true else false
  | None , None -> true
  | _, _ -> false
;;

assert(test_float (find_zero (parse "(x*2)-8") 3. 0.001 5) (Some 4.) 0.01);;
assert(test_float (find_zero (parse "(x^2)-8") 3. 0.001 5) (Some 2.828) 0.001);;
assert(test_float (find_zero (parse "10 - (2*x)") 3. 0.001 5) (Some 5.) 0.001);;


(*>* Problem 2.5 *>*)

(* Challenge problem:
 * Just leave it unimplemented if you don't want to do it.
 * See writeup for instructions. *)
(*
let rec find_zero_exact (e:expression) : expression option =
    failwith "Not implemented"
;;

 *)
(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 : int = 600;;
