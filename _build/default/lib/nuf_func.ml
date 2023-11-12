open! Batteries
open Ptmap
open Either
open Tuple2

module type Lens = sig
  type ('a, 'b) t
  val getl : ('a, 'b) t -> 'a
  val mapl : ('a -> 'a) -> ('a, 'b) t -> ('a, 'b) t
  (* val mapl_lst : ('a -> 'a list) -> ('a, 'b) t -> ('a, 'b) t list *)
end

module Make(L : Lens) : sig
  type 'a nuf = 'a puf list
  and 'a puf

  val search : int -> ('a puf, 'b) L.t -> int * 'a
  val search_all : int -> ('a puf, 'b) L.t list -> (int * 'a) list
  val merge_det : ('a -> 'a -> ('a puf, 'b) L.t -> 'a * ('a puf, 'b) L.t) -> 
    int -> int -> ('a puf, 'b) L.t -> ('a puf, 'b) L.t
  val mergei : (int -> 'a -> int -> 'a -> ('a puf, 'b) L.t -> ('a * ('a puf, 'b) L.t) list) -> 
    int -> int -> ('a puf, 'b) L.t -> ('a puf, 'b) L.t list
  val merge : ('a -> 'a -> ('a puf, 'b) L.t -> ('a * ('a puf, 'b) L.t) list) -> 
    int -> int -> ('a puf, 'b) L.t -> ('a puf, 'b) L.t list
  val set_det : int -> 'a -> ('a puf, 'b) L.t -> ('a puf, 'b) L.t
  val update_det : int -> ('a -> 'a) -> ('a puf, 'b) L.t -> ('a puf, 'b) L.t
  val update_nondet : int -> ('a -> 'a list) -> ('a puf, 'b) L.t -> ('a puf, 'b) L.t list
  val empty : 'a puf
  val (<|>) : 'a nuf -> 'a nuf -> 'a nuf
  val pure : 'a puf -> 'a nuf
  val add_det : int -> 'a -> ('a puf, 'b) L.t -> ('a puf, 'b) L.t
end = struct

  let (let&) x f = L.mapl f x
  (* let (let$) x f = L.mapl_lst f x *)
  
  let ( let+ ) x f = List.map f x
  (* let ( let* ) x f = List.(map f x |> flatten) *)
  (* let (<&>) = ( let+ ) *)
  (* let (>>=) = ( let* ) *)

  let modify k f = update k @@ function
    | Some x -> Some (f x)
    | None -> raise @@ Invalid_argument "ptmap modify"

  type 'a nuf = 'a puf list
  and 'a puf = ('a, int) Either.t Ptmap.t * int Ptmap.t

  let search x t = 
    let rec go x t = match find x @@ fst t with
      | Left v -> x, v
      | Right i -> go i t in
    go x (L.getl t)

  let search_all x = List.map (search x)

  let merge_det sel x y puf = 
    let ix, vx = search x puf
    and iy, vy = search y puf in
    let v, t = sel vx vy puf in
    let& t = t in
    if ix = iy then t
    else match Stdlib.compare (find x @@ snd t) (find y @@ snd t) with
      | 1 -> map1 (add iy (Right ix) % add ix (Left v)) t
      | -1 -> map1 (add ix (Right iy) % add iy (Left v)) t
      | _ -> map (add ix (Right iy) % add iy (Left v)) (modify iy succ) t

  let mergei sel x y puf = 
    let ix, vx = search x puf
    and iy, vy = search y puf in
    let+ v, t = sel ix vx iy vy puf in
    let& t = t in
    if ix = iy then t
    else match Stdlib.compare (find x @@ snd t) (find y @@ snd t) with
      | 1 -> map1 (add iy (Right ix) % add ix (Left v)) t
      | -1 -> map1 (add ix (Right iy) % add iy (Left v)) t
      | _ -> map (add ix (Right iy) % add iy (Left v)) (modify iy succ) t

  let merge sel x y puf = 
    let ix, vx = search x puf
    and iy, vy = search y puf in
    let+ v, t = sel vx vy puf in
    let& t = t in
    if ix = iy then t
    else match Stdlib.compare (find x @@ snd t) (find y @@ snd t) with
      | 1 -> map1 (add iy (Right ix) % add ix (Left v)) t
      | -1 -> map1 (add ix (Right iy) % add iy (Left v)) t
      | _ -> map (add ix (Right iy) % add iy (Left v)) (modify iy succ) t

  let set_det x v puf = 
    let i, _ = search x puf in
    let& puf = puf in
    map1 (add i (Left v)) puf

  let update_det x f puf = 
    let i, v = search x puf in
    let& puf = puf in
    map1 (add i (Left (f v))) puf

  let update_nondet x f puf = 
    let i, v1 = search x puf in
    let+ v2 = f v1 in
    let& puf = puf in
    map1 (add i (Left v2)) puf

  let (<|>) = (@)
  let pure puf = [puf]

  let empty = empty, empty
  let add_det x v = L.mapl (map (add x (Left v)) (add x 0))

end

type ('a, 'b) megapuf = {
  monoid : 'a;
  letter : 'b;
}

module Monoid = Make(struct
  type ('a, 'b) t = ('a, 'b) megapuf
  let getl x = x.monoid
  let mapl f x = {x with monoid = f x.monoid}
  (* let mapl_lst f x = 
    f x.monoid |> List.map (fun y -> {x with monoid = y}) *)
end)

module Letter = Make(struct
  type ('a, 'b) t = ('b, 'a) megapuf
  let getl x = x.letter
  let mapl f x = {x with letter = f x.letter}
  (* let mapl_lst f x = 
    f x.letter |> List.map (fun y -> {x with letter = y}) *)
end)
