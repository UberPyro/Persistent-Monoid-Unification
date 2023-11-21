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
  
  let front2 d = Option.map (fun (h, t) -> h, t, D.front t) (D.front d)
  let rear2 d = Option.map (fun (f, r) -> D.rear f, f, r) (D.rear d)

  let nullify i p = set_det i D.empty p
  let null_all vs p = List.fold_left (fun p j -> set_det j D.empty p) p vs
  
  let fresh p = 
    let k = unique () in
    Var k, add_det k D.(cons (Var k) empty) p
  
  let rec solve hp d1_ d2_ p0_ = 
    let d1, p1_ = simp p0_ d1_ in
    let d2, p0 = simp p1_ d2_ in
    match[@warning "-57"] front2 d1, front2 d2, rear2 d1, rear2 d2 with
    | Some (Var i, _, None), _, _, _  | _, Some (Var i, _, None), _, _ -> 
      let occurs, diff = List.partition (function
        | Var j -> i = j
        | _ -> false
      ) (D.to_list d2) in
      let cs, vars = List.partition_map (function 
        | Atom _ -> Left () 
        | Var j -> Right j
      ) diff in
      begin match occurs, cs with
        | [], _ -> [set_det i d2 p0]
        | _ :: _, () :: _ -> []
        | [_], [] -> [null_all vars p0]
        | _ :: _ :: _, [] -> [null_all (i :: vars) p0]
      end
    | None, None, _, _ -> [p0]

    | None, Some (Atom _, _, _), _, _ | Some (Atom _, _, _), None, _, _
    | _, _, None, Some (_, _, Atom _) | _, _, Some (_, _, Atom _), None -> []

    | Some (Atom a, u, _), Some (Atom b, v, _), _, _ 
    | _, _, Some (_, u, Atom a), Some (_, v, Atom b) -> 
      List.concat_map (solve hp u v) (A.unify a b p0)

    | None, Some (Var i, u, _), _, _ | Some (Var i, u, _), None, _, _
    | _, _, None, Some (_, u, Var i) | _, _, Some (_, u, Var i), None -> 
      solve hp u d2 (nullify i p0)
    
    | Some (Var i, u, _), Some (Var j, v, _), _, _
    | _, _, Some (_, v, Var j), Some (_, u, Var i) when i = j -> solve hp u v p0

    | _ when hp <= 0 -> []

    | Some (Atom _ as a, u, _), Some (Var i, v, _), _, _
    | Some (Var i, v, _), Some (Atom _ as a, u, _), _, _ -> 
      let k, p1 = fresh p0 in
      solve (hp-1) (D.cons a u) v (nullify i p1)
    @ solve (hp-1) u (D.cons k v) (set_det i D.(cons a (cons k empty)) p1)    
    
    | _, _, Some (_, u, (Atom _ as a)), Some (_, v, Var i)
    | _, _, Some (_, v, Var i), Some (_, u, (Atom _ as a)) -> 
      let k, p1 = fresh p0 in
      solve (hp-1) (D.snoc u a) v (nullify i p1)
    @ solve (hp-1) (D.snoc u k) v (set_det i D.(cons k (cons a empty)) p1)    
    
    | Some (Var i as w, x, Some ((Atom _ as a), u)), Some (Var j as y, v, _), _, _
    | Some (Var j as y, v, _), Some (Var i as w, x, Some ((Atom _ as a), u)), _, _ -> 
      let n, p1 = fresh p0 in
      solve (hp/2) u D.(cons n v) (set_det j D.(cons w (cons a (cons n empty))) p1)
    @ solve (hp/2) D.(cons n x) v (set_det i D.(cons y (cons n empty)) p1)
    
    | _, _, Some (Some (u, (Atom _ as a)), x, (Var i as w)), Some (Some (_, Var _), v, (Var j as y))
    | _, _, Some (Some (_, Var _), v, (Var j as y)), Some (Some (u, (Atom _ as a)), x, (Var i as w)) -> 
      let n, p1 = fresh p0 in
      solve (hp/2) u D.(snoc v n) (set_det j D.(snoc (snoc (cons n empty) a) w) p1)
    @ solve (hp/2) D.(snoc x n) v (set_det i D.(snoc (cons n empty) y) p1)
    
    | Some (Var i as vi, u, Some (Var _, _)), Some (Var j as vj, v, Some (Var _, _)), _, _ -> 
      let n, p1 = fresh p0 in
      solve (hp/2) D.(cons n u) v (set_det i D.(cons vj (cons n empty)) p1)
    @ let a, p2 = T2.map1 (fun z -> Atom z) (A.fresh p1) in
      solve (hp/2) u D.(cons a (cons n v))
        (set_det j D.(cons vi (cons a (cons n empty))) p2)
  
  let unify hp = merge (fun x y -> solve hp x y %> List.map (flip simp x))

end
