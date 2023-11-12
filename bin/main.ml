open! Batteries
open Pmu
open! U
open! Glue
open! M
module P = Nuf_func.Monoid

let () = match Sys.argv.(1) with
  | "simplify" -> 
    let lexbuf = Lexing.from_string (Sys.argv.(2) ^ "$") in
    begin match Parse.expr_file Lex.token lexbuf with
      | [] -> print_endline "<eps>"
      | t :: ts -> 
        print_term t; 
        List.iter (fun t -> print_string " "; print_term t) ts
    end;
    print_newline ()
  | "unify" -> 
    let lexbuf = Lexing.from_string (Sys.argv.(2) ^ "$") in
    let (e1, e2) = 
      Parse.problem Lex.token lexbuf
      |> Tuple2.mapn (fun ts -> Expr ts) in
    let i, j = unique (), unique () in
    let p = Nuf_func.{monoid=P.empty; letter=Nuf_func.Letter.empty} in
    let p0 = 
      Hashtbl.values m
      |> List.of_enum
      |> List.fold_left (fun a i -> P.add_det i Var a) p in
    let p1 = P.add_det i e1 p0 in
    let p2 = P.add_det j e2 p1 in
    let p3 = unify 128 i j p2 in
    print_monoid (i, p3);
    print_newline ();
    (* p3 |> List.iter @@ fun p -> 
      print_string "Unifier 1: ";
      print_monoid (i, p) *)
  | s -> failwith (Printf.sprintf "[%s] is not a recognized operation" s)
