open! Batteries
open Printf
module T2 = Tuple2
(* module E = Either *)
module D = Deque

open Nuf_func.Monoid
open U

module Make(A : UNIFIABLE) = struct  (* plotkin semialgorithm + pyro heuristics *)
  
  type t = term D.t
  and term = 
    | Atom of A.t
    | Var of int
  
  let rec simp ?(acc=D.empty) p0 = D.fold_left (fun (d1, p1) -> function
    | Atom _ as a -> D.snoc d1 a, p1
    | Var i -> 
      let j, t1 = search i p0 in
      let t2, p2 = simp ~acc:d1 p0 t1 in
      t2, set_det j t2 p2
  ) (acc, p0)

  let print_term out = function
    | Atom a -> fprintf out "%s" (A.to_string a)
    | Var i -> fprintf out "[%d]" i

  let print_monoid out p0 = simp p0 %> fst %> D.front %> function[@warning "-8"]
    | None -> fprintf out "\n"
    | Some (h, t) -> 
      print_term out h;
      D.iter (fun t -> fprintf out " "; print_term out t) t
  
  let front2 d = Option.map (fun (h, t) -> h, D.front t) (D.front d)
  let rear2 d = Option.map (fun (f, r) -> D.rear f, r) (D.rear d)

  let nullify i p = set_det i D.empty p
  let null_all vs p = List.fold_left (fun p j -> set_det j D.empty p) p vs
  let recons = function
    | Some (h, t) -> D.cons h t
    | None -> D.empty
  let resnoc = function
    | Some (f, r) -> D.snoc f r
    | None -> D.empty
  
  let fresh p = 
    let k = unique () in
    k, add_det k D.(cons (Var k) empty) p
  
  let rec solve hp d1_ d2_ p0_ = 
    let d1, p1_ = simp p0_ d1_ in
    let d2, p0 = simp p1_ d2_ in
    match front2 d1, front2 d2, rear2 d1, rear2 d2 with
    | Some (Var i, None), _, _, _  | _, Some (Var i, None), _, _ -> 
      let occurs, diff = List.partition (function
        | Var j -> i = j
        | _ -> false
      ) (D.to_list d2) in
      let cs, vars = List.partition_map (function 
        | Atom _ -> Left () 
        | Var j -> Right j
      ) diff in
      begin match[@warning "-8"] occurs, cs with
        | [], _ -> [set_det i d2 p0]
        | _ :: _, () :: _ -> []
        | [_], [] -> [null_all vars p0]
        | _ :: _ :: _, [] -> [null_all (i :: vars) p0]
      end
    | None, None, _, _ -> [p0]

    | None, Some (Atom _, _), _, _ | Some (Atom _, _), None, _, _
    | _, _, None, Some (_, Atom _) | _, _, Some (_, Atom _), None -> []

    | Some (Atom a, u), Some (Atom b, v), _, _ -> 
      List.concat_map (solve hp (recons u) (recons v)) (A.unify a b p0)
    | _, _, Some (u, Atom a), Some (v, Atom b) -> 
      List.concat_map (solve hp (resnoc u) (resnoc v)) (A.unify a b p0)

    | None, Some (Var i, u), _, _ | Some (Var i, u), None, _, _ -> 
      solve hp (recons u) d2 (nullify i p0)
    | _, _, None, Some (u, Var i) | _, _, Some (u, Var i), None -> 
      solve hp (resnoc u) d2 (nullify i p0)

    | _ when hp <= 0 -> []

    | Some (Atom _ as a, u), Some (Var i, v), _, _
    | Some (Var i, v), Some (Atom _ as a, u), _, _ -> 
      let k, p1 = fresh p0 in
      solve (hp-1) (D.cons a (recons u)) (recons v) (nullify i p1)
    @ solve (hp-1) (recons u) (recons v) (set_det i D.(cons a (cons (Var k) empty)) p1)    
    
    | _, _, Some (u, (Atom _ as a)), Some (v, Var i)
    | _, _, Some (v, Var i), Some (u, (Atom _ as a)) -> 
      let k, p1 = fresh p0 in
      solve (hp-1) (D.cons a (resnoc u)) (resnoc v) (nullify i p1)
    @ solve (hp-1) (resnoc u) (resnoc v) (set_det i D.(cons (Var k) (cons a empty)) p1)    
    
    | Some (Var i, Some ((Atom _ as a), u)), Some (Var j, Some (Var k, v)), _, _
    | Some (Var j, Some (Var k, v)), Some (Var i, Some ((Atom _ as a), u)), _, _ -> 
      let n, p1 = fresh p0 in
      solve (hp/2) u D.(cons (Var n) (cons (Var k) v))
        (set_det j D.(cons (Var i) (cons a (cons (Var n) empty))) p1)
    
    | _, _, Some (Some (u, (Atom _ as a)), Var i), Some (Some (v, Var k), Var j)
    | _, _, Some (Some (v, Var k), Var j), Some (Some (u, (Atom _ as a)), Var i) -> 
      let n, p1 = fresh p0 in
      solve (hp/2) u D.(cons (Var n) (cons (Var k) v))
        (set_det j D.(cons (Var i) (cons a (cons (Var n) empty))) p1)
    
    | _ -> failwith "todo"

  

end
