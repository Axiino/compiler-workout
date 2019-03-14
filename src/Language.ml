(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
     *)                                                       
    let bool_of_int intValue = intValue != 0
    let int_of_bool bolValue = if bolValue then 1 else 0

    let eval_operator operator left right = match operator with
      | "!!" -> int_of_bool (bool_of_int left || bool_of_int right)
      | "&&" -> int_of_bool (bool_of_int left && bool_of_int right)
      | "==" -> int_of_bool (left = right)
      | "!=" -> int_of_bool (left != right)
      | "<=" -> int_of_bool (left <= right)
      | "<"  -> int_of_bool (left < right)
      | ">=" -> int_of_bool (left >= right)
      | ">"  -> int_of_bool (left > right)
      | "+"  -> left + right
      | "-"  -> left - right
      | "*"  -> left * right
      | "/"  -> left / right
      | "%"  -> left mod right

    let rec eval state expr = match expr with
      | Const v -> v
      | Var x -> state x
      | Binop (operator, left, right) -> eval_operator operator (eval state left) (eval state right)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    let parse_binop operator = ostap(- $(operator)), (fun x y -> Binop (operator, x, y))

    ostap (
      expr:
        !(Ostap.Util.expr
          (fun x -> x)
          (Array.map (fun (a, operator) -> a, List.map parse_binop operator)
            [|
              `Lefta, ["!!"];
              `Lefta, ["&&"];
              `Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
              `Lefta, ["+"; "-"];
              `Lefta, ["*"; "/"; "%"];
            |]
          )
          primary
          );
      primary:
          c: DECIMAL {Const c}
        | x: IDENT {Var x}
        | -"(" expr -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval conf operator =
      let (st, input, output) = conf in
      match operator with
        | Read var	-> (match input with
                | x::rest -> (Expr.update var x st), rest, output
                | [] -> failwith("input error")
        )
        | Write exp -> st, input, (output @ [Expr.eval st exp])
        | Assign (var, exp) -> (Expr.update var (Expr.eval st exp) st), input, output
        | Seq (f, s) -> eval (eval conf f) s

    (* Statement parser *)
    ostap (
      stmt:
			x:IDENT ":=" e:!(Expr.expr) {Assign(x, e)}
			  | "read" "(" x:IDENT ")" {Read x}
			  | "write" "(" e:!(Expr.expr) ")" {Write e};
  		parse: s:stmt ";" rest:parse {Seq(s, rest)} | stmt
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
