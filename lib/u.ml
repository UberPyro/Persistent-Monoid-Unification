open! Batteries

open Nuf_func.Letter

module type UNIFIABLE = sig
  type t

  (* change eventually to hof *)
  val unify : t -> t -> ('a, t puf) Nuf_func.megapuf -> ('a, t puf) Nuf_func.megapuf list
  val freeze : t -> t
  val to_string : t -> string
end

module Letter = struct
  type t = string
  let unify x y p = 
    if x = y then [p]
    else []
  let freeze = Fun.id
  let to_string = Fun.id
end
