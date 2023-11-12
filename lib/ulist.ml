open! Batteries
open Printf
module T2 = Tuple2
module E = Either

open Nuf_func.Monoid
open U

module Make(A : UNIFIABLE) = struct  
  (* type t = int *)
  type _t = 
    | Var
    | Expr of term list
  and term = 
    | Letter of A.t
    | Monoid of int
  
  let bind_puf f (e, puf) = 
    List.fold_left (fun (a, p) t -> 
      let t', p' = f t p in
      List.rev_append t' a, p'
    ) ([], puf) e |> T2.map1 List.rev
  
  let (let*) xs f = bind_puf (curry f) xs

  let rec flatten x = 
    let* (term, puf) = x in
    match term with
      | Letter a -> [Letter a], puf
      | Monoid i -> flatten_monoid (i, puf)
  
  and flatten_monoid (i, puf) = match search i puf with
    | j, Var -> [Monoid j], puf
    | j, Expr ts -> 
      let ts', p' = flatten (ts, puf) in
      ts', set_det j (Expr ts') p' 
  
  let freeze = 
    let c = ref (-1) in
    let next () = c := succ !c; !c in
    let m = Hashtbl.create 32 in
    let memo i = 
      Hashtbl.find_option m i |> Option.default_delayed @@ fun () -> 
        let nu = next () in
        Hashtbl.add m i nu;
        nu in
    List.map @@ function
      | Monoid i -> Monoid (memo i)
      | Letter a -> Letter a (* need to freeze letters as well *)
  
  let uniquify = 
    let m = Hashtbl.create 32 in
    let log = freeze %> fun y -> 
      match Hashtbl.find_option m y with
      | Some () -> false
      | None -> 
        Hashtbl.add m y ();
        true in
    List.filter log
  
  let nullify i = set_det i (Expr [])
  
  let solve_trivial_var i e p = 
    let consts, vars = List.partition_map (function 
      | Letter a -> Left a
      | Monoid i -> Right i
    ) e in
    let occurs, others = List.partition ((=) i) vars in
    match List.length occurs with
      | 0 -> [Expr e, p]
      | _ when List.length consts > 0 -> []
      | 1 -> [Var, List.fold_left (flip nullify) p others]
      | _ -> [Expr [], List.fold_left (flip nullify) p others]
  
  let replace_sol e1 = List.map (T2.map1 (const (Expr e1)))
  
  let rec solve n e1 e2 p0 = 
    let (e3, p1) = flatten (e1, p0) in
    let (e4, p ) = flatten (e2, p1) in
    match e3, e4 with
    | [], [] -> [p]
    | [Monoid i], e | e, [Monoid i] -> 
      List.map (fun (v, p1) -> set_det i v p1) (solve_trivial_var i e p)
    | Letter _ :: _, [] | [], Letter _ :: _ -> []
    | Letter i :: ts, Letter j :: us -> List.concat_map (solve n ts us) (A.unify i j p)
    | Monoid i :: ts, [] | [], Monoid i :: ts -> solve n ts [] (nullify i p)
    | Monoid i :: ts, Monoid j :: us when i = j -> solve n ts us p
    | _ when n <= 0 -> []
    | (Monoid i :: _ as ts), (Letter a :: _ as us)
    | (Letter a :: _ as us), (Monoid i :: _ as ts) -> 
      solve n ts us (nullify i p)
    @ let j = unique () in
      solve (n-1) ts us (set_det i (Expr [Letter a; Monoid j]) (add_det j Var p))
    | (Monoid i :: _ as ts), (Monoid j :: _ as us) -> 
      let k = unique () in
      solve (n/2) ts us (set_det i (Expr [Monoid j; Monoid k]) (add_det k Var p))
    @ solve (n/2) ts us (set_det j (Expr [Monoid i; Monoid k]) (add_det k Var p))

  let unify n = mergei @@ fun i u j v p -> match (i, u), (j, v) with
    | (_, Var), (_, Var) -> [Var, p]
    | (i, Var), (_, Expr e) | (_, Expr e), (i, Var) -> solve_trivial_var i e p
    | (_, Expr e1), (_, Expr e2) -> 
      List.map
        (fun p -> T2.map1 (fun e -> Expr (freeze e)) (flatten (e1, p)))
        (solve n e1 e2 p)

  let pretty_term out term = match term with
    | Letter a -> fprintf out "%s" (A.to_string a)
    | Monoid i -> fprintf out "[%d]" i

  let pretty_monoid out k nuf = 
    search_all k nuf |> function
    | [] -> fprintf out "%s" "no unifiers.\n"
    | lst -> 
      List.iter2i (fun k (i, x) p -> 
        fprintf out "#%d: " (succ k);
        (match x with
        | Var -> fprintf out "[%d]" i
        | Expr e -> match flatten (e, p) |> fst with
          | [] -> fprintf out "%s" "<eps>"
          | t :: ts -> 
            pretty_term out t;
            List.iter (fun t -> fprintf out "%s" " "; pretty_term out t) ts
        ); fprintf out "%s" "\n"
      ) lst nuf
  
  let printerfy f x = 
    let out = IO.output_string () in
    f out x;
    print_string (IO.close_out out)
  
  let print_term = printerfy pretty_term
  let print_monoid eps = 
    printerfy (fun out (x, p) -> pretty_monoid out x p) eps

end
