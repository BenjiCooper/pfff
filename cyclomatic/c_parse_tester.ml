(* Author: Benjamin Cooper
   Tool to print results of parsing C code in a readable fashion to better 
   understand structure of the returned AST. *)


open Ast_c
open Parse_c

let get_name (n,t) = n;;

let rec print_expr ex = match ex with
	  Int(sw) -> print_string (get_name sw)
	| Float(sw) -> print_string (get_name sw)
	| String(sw) -> print_string (get_name sw)
	| Char(sw) -> print_string (get_name sw)

	| Id(n) -> print_string (get_name n)

	| CondExpr(e1,e2,e3) -> print_string "cond\n"

	| _ -> print_string "expr\n"


let rec print_stmt st = match st with
	  ExprSt(e) -> print_expr e
	| Block(sl) -> List.fold_left (fun a b -> print_stmt b ; a) () sl

	| If(e,s1,s2) -> print_string " if (" ;
					 print_expr e ;
					 print_string ") then ";
					 print_stmt s1 ;
					 print_string " else " ;
					 print_stmt s2 ;
					 print_string "\n"
	| Switch(e,cl) -> print_string "switch"

	| _ -> print_string "stmt"
	; print_string "\n"

let print_type ty = print_string "type "

let rec print_toplevel tl = match tl with
	  Include(s) -> print_string (get_name s) ; print_string "\n"
	| FuncDef(fd) -> List.fold_left (fun a b -> print_stmt b ; a) () fd.f_body

let print_any any = match any with
	  Expr(ex) -> print_expr ex
	| Stmt(st) -> print_stmt st
	| Type(ty) -> print_type ty
	| Toplevel(tl) -> print_toplevel tl
	| Program(pr) -> List.fold_left (fun a b -> print_toplevel b ; a) () pr

let printer filename = 
	let ast = parse_program filename in
	List.fold_left (fun a b -> print_toplevel b ; a) () ast
	; print_string "\n"
;;

let () = 
	let fn = Array.get (Sys.argv) 1 in
	printer fn
;;