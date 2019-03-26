open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         
let rec eval env ((stack, ((st, i, o) as c)) as conf) = function
  | [] -> conf
  | inst :: prog_tail ->
       match inst with
       | BINOP op ->
          begin
            match stack with
            | y :: x :: tail ->
               eval env ((Expr.eval_binop op x y) :: tail, c) prog_tail
            | _ -> failwith "cannot perform BINOP"
          end
       | CONST v -> eval env (v :: stack, c) prog_tail
       | READ ->
          begin
            match i with
            | x :: tail -> eval env (x :: stack, (st, tail, o)) prog_tail
            | _ -> failwith "cannot perform READ"
          end
       | WRITE ->
          begin
            match stack with
            | x :: tail -> eval env (tail, (st, i, o @ [x])) prog_tail
            | _ -> failwith "cannot perform WRITE"
          end
       | LD x -> eval env ((st x) :: stack, c) prog_tail
       | ST x ->
          begin
            match stack with
            | z :: tail -> eval env (tail, ((Expr.update x z st), i, o)) prog_tail
            | _ -> failwith "cannot perform ST"
          end
       | LABEL l -> eval env conf prog_tail
       | JMP l -> eval env conf (env#labeled l)
       | CJMP (b, l) ->
          begin
            match stack with
            | x :: tail -> if (x = 0 && b = "z" || x != 0 && b = "nz")
                           then eval env (tail, c) (env#labeled l)
                           else eval env (tail, c) prog_tail
            | _ -> failwith "stack is empty"
          end

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
class labels =
  object (self)
    val label_n = 0
    method get_label = {< label_n = label_n + 1 >}, self#generate_label
    method generate_label = "label" ^ string_of_int label_n
  end

let rec compile_expr e = match e with
    | Language.Expr.Const x -> [CONST x]
    | Language.Expr.Var n -> [LD n]
    | Language.Expr.Binop (op, e1, e2) -> compile_expr e1 @ compile_expr e2 @ [BINOP op]

let rec compile_impl lb p after_label = match p with
    | Language.Stmt.Read name -> ([READ; ST name]), false, lb
    | Language.Stmt.Write expr -> (compile_expr expr @ [WRITE]), false, lb
    | Language.Stmt.Assign (name, expr) -> (compile_expr expr @ [ST name]), false, lb
    | Language.Stmt.Seq (e1, e2) -> let (lb, label) = lb#get_label in
                                    let (prg1, used1, lb) = compile_impl lb e1 label in
                                    let (prg2, used2, lb) = compile_impl lb e2 after_label in
                                    (prg1 @
                                    (if used1 then [LABEL label] else []) @
                                    prg2), used2, lb
    | Language.Stmt.Skip -> [], false, lb
    | Language.Stmt.If (cond, thn, els) ->
        let lb, else_label = lb#get_label in
        let condition = compile_expr cond in
        let (thn_body, used1, lb) = compile_impl lb thn after_label in
        let (els_body, used2, lb) = compile_impl lb els after_label in
        condition @ [CJMP ("z", else_label)] @
        thn_body @ (if used1 then [] else [JMP after_label]) @ [LABEL else_label] @
        els_body @ (if used2 then [] else [JMP after_label])
        , true, lb
    | Language.Stmt.While (cond, body) -> 
        let lb, before_label = lb#get_label in
        let lb, condition_label = lb#get_label in
        let do_body, _, lb = compile_impl lb body condition_label in
        let condition = compile_expr cond in
        [JMP condition_label; LABEL before_label] @
        do_body @ [LABEL condition_label] @ condition @ [CJMP ("nz", before_label)]
        , false, lb
    | Language.Stmt.RepeatUntil (body, cond) -> 
        let (prg, _, lb) = compile_impl lb (Language.Stmt.While (
                                            Language.Stmt.reverse_condition cond, body)) after_label in
        List.tl (prg), false, lb

let rec compile p = let lb, label = (new labels)#get_label in
                    let prg, used, _ = compile_impl lb p label in
                    prg @ (if used then [LABEL label] else [])
