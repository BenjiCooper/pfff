(* Author: Benjamin Cooper, benji97@gmail.com
 * This code was written to perform analysis for code entries to the
 * Build it, Break it, Fix it (BIBIFI) competition.
 * This specific file converts C code into a control flow graph
 * in order to perform various analyses.
 *)


(* As it turns out, this file is riddled with bugs. *)
(* Debug by looking at what the Ast looks like after parsing. *)

open Ast_c
open Graph
open Parse_c

(* STATEMENT CONVERSION TO GRAPH *)

let bhash1 = Hashtbl.create 20;; (* Used for contains_break *)
(*let bhash2 = Hashtbl.create 20;;*) (* Used for binding break statements *)
let ghash = Hashtbl.create 20;; (* Assume there's -hopefully- fewer than 20 GOTO statements *)

(* Does the given statement contain a break? *)
let rec contains_break stmt = match stmt with

      Block(b) -> foldl (fun a x -> a || (contains_break x)) false b

    | If(e,s1,s2) -> (contains_break s1) || (contains_break s2)
    (* NOTE: maybe switch should just return true? *)
    | Switch(e,cl) -> let case_to_break c = (match c with
                            Case(e,sl) -> foldl (fun a b -> a || (contains_break b)) false sl
                          | Default(sl) -> foldl (fun a b -> a || (contains_break b)) false sl) in 
        foldl (fun a b -> a || (case_to_break b)) false cl
    | While(e,s) -> contains_break s
    | DoWhile(s,e) -> contains_break s
    | For(e1,e2,e3,s) -> contains_break s

    | Continue -> true
    | Break -> true

    | Label(n,s) -> Hashtbl.add bhash1 n s ; contains_break s
    | Goto(n) -> let s = Hashtbl.find bhash1 n in contains_break s

    | _ -> false



let rec stmt_to_graph st = 

    (*let break = ref -1 in*)

    match st with

      Block(b) -> let n = next () in foldr (fun x a -> connect a (stmt_to_graph x)) b {nodes=[n];edges=[];head=n;tail=n}

    (* Branch statements *)
    (* Needs to be updated to account for compound if statements *)
    | If(e,s1,s2) -> let h = next () in
                     let g1 = stmt_to_graph s1 in
                     let g2 = stmt_to_graph s2 in
                     let t = next () in
                     let nodes = [h;t]@(g1.nodes)@(g2.nodes) in
                     let e1 = {src=h;dst=g1.head;} in
                     let e2 = {src=h;dst=g2.head;} in
                     let e3 = {src=g1.tail;dst=t;} in
                     let e4 = {src=g2.tail;dst=t;} in
                     let edges = [e1;e2;e3;e4]@(g1.edges)@(g2.edges) in
                     { nodes = nodes; edges = edges; head = h; tail = t; }
    | Switch(e,cl) -> let case_to_g c = (match c with
                             Default(sl) -> let n = next () in foldr (fun x a -> connect a (stmt_to_graph x)) sl {nodes=[n];edges=[];head=n;tail=n}
                           | Case(e,sl) -> let n = next () in foldr (fun x a -> connect a (stmt_to_graph x)) sl {nodes=[n];edges=[];head=n;tail=n}) in
                      let l = map (case_to_g) cl in
                      let h = next () in
                      let t = next () in
                      let g = {nodes=[h;t];edges=[];head=h;tail=t;} in
                      let btwn gr1 gr2 =
                          let nodes = gr1.nodes@gr2.nodes in
                          let e1 = {src=gr1.head;dst=gr2.head;} in
                          let e2 = {src=gr2.tail;dst=gr1.tail;} in
                          let edges = [e1;e2]@gr1.edges@gr2.edges in
                          { nodes = nodes; edges = edges; head = h; tail = t; }
                      in foldl (btwn) g l
    | While(e,s) -> (* Set up the graph to be returned *)
                    let h = next () in
                    let g = stmt_to_graph s in
                    let t = next () in
                    let nodes = [h;t]@g.nodes in
                    let e1 = {src=h;dst=g.head;} in
                    let e2 = {src=g.tail;dst=h;} in
                    let e3 = {src=h;dst=t;} in
                    let edges = [e1;e2;e3]@g.edges in
                    let graph = { nodes = nodes; edges = edges; head = h; tail = t; } in
                    (* Deal with any break statements *)
                    (* Currently does not support breaks in nested loops *)
                    (*let b = contains_break s in
                    let u = if b then let n = next () in Hashtbl.add bhash2 n t; break := n; () else () in*)
                    graph
    | DoWhile(s,e) -> let g = stmt_to_graph s in
                      let c = next () in
                      let t = next () in
                      let nodes = [c;t]@g.nodes in
                      let e1 = {src=g.tail;dst=c;} in
                      let e2 = {src=c;dst=g.head;} in
                      let e3 = {src=c;dst=t;} in
                      let edges = [e1;e2;e3]@g.edges in
                      { nodes = nodes; edges = edges; head = g.head; tail = t; }
    | For(ex1,ex2,ex3,s) -> let h = next () in
                            let g = stmt_to_graph s in
                            let t = next () in
                            let nodes = [h;t]@g.nodes in
                            let e1 = {src=h;dst=g.head;} in
                            let e2 = {src=g.tail;dst=h;} in
                            let e3 = {src=h;dst=t;} in
                            let edges = [e1;e2;e3]@g.edges in
                            {nodes = nodes; edges = edges; head = h; tail = t; }

    (* GOTO label break and continue do not work yet. They need to be fixed. *)

    (* Half-baked solution to break & continue *)
    | Break -> failwith "unimplemented" (*let n = Hashtbl.find (!break in
                   { nodes = [!break]; edges = [{src=(!break);dst=n;}]; head = n; tail = n }*)
    | Continue -> failwith "unimplemented"

    (* Use a hash to create associations for Label and GOTO *)
    | Label(n,s) -> let c = next () in
                    Hashtbl.add ghash n c ;
                    { nodes = [c]; edges = []; head = c; tail = c; }
    | Goto(n) -> let n1 = Hashtbl.find ghash n in
                 let n2 = next () in
                 let e = {src=n2;dst=n1;} in
                 { nodes = [n2]; edges = [e]; head = n2; tail = n2; }

    (* Anything else just becomes a single node *)
    (* NOTE: This probably won't work for ASM *)
    (* NOTE: This needs to be updated to account for conditional expressions (are those ternaries?) *)
    | _ -> let n = next () in
           { nodes = [n]; edges = []; head = n; tail = n; }
;;

(* TOP-LEVEL CONVERSION TO GRAPH *)

let func_to_graph { f_name = n; f_type = t; f_body = b; f_static = s; } = 
    let n = next () in foldr (fun x a -> connect a (stmt_to_graph x)) b {nodes=[n];edges=[];head=n;tail=n}
;;

let rec top_level_to_graph tl = match tl with
      FuncDef(fd) -> func_to_graph fd
    | _ -> empty_graph
;;

(* PROGRAM CONVERSION TO GRAPH *)

(* This may be preferable. *)
let program_to_graph_list pr = map (fun a -> top_level_to_graph a) pr;;

(* This function is problematic, and shouldn't be used quite yet. *)
let rec program_to_graph pr = match pr with
      [] -> empty_graph
    | h::t -> let g1 = top_level_to_graph h in
              let g2 = program_to_graph t in
              (* This line is problematic, because of head and tail. Maybe I should return a graph list? *)
              { nodes = g1.nodes@g2.nodes; edges = g1.edges@g2.edges; head = g1.head; tail = g2.tail }
;;

(* CODE CONVERSION TO GRAPH. This data type seems to be superfluous outside of testing... *)

let rec to_graph code = match code with
      Expr(ex) -> empty_graph
    | Stmt(st) -> stmt_to_graph st
    | Type(ty) -> empty_graph
    | Toplevel(tl) -> top_level_to_graph tl
    | Program(pr) -> program_to_graph pr
;;

let num_funs pr = 
    let is_fun tl = (match tl with
          FuncDef(f) -> true
        | _ -> false) in
    foldl (fun a b -> if is_fun b then 1 + a else a) 0 pr
;;

let analyze filename = 
    let ast = parse_program filename in
    let grl = program_to_graph_list ast in
    let n = foldl (fun a b -> a + (List.length b.nodes)) 0 grl in
    let e = foldl (fun a b -> a + (List.length b.edges)) 0 grl in
    let p = num_funs ast in
    let cc = e - n + 2*p in
    foldl (fun a g -> print_graph g; print_string "\n"; a) () grl ;
    print_string "\nnum funcs: ";
    print_int p;
    print_string "\ncyclomatic complexity: ";
    print_int cc;
    print_string"\n"
;;

let () = 
    let fn = Array.get (Sys.argv) 1 in
    analyze fn
;;

