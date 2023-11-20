open! Batteries

open Nuf_func.Letter

module type UNIFIABLE = sig
  type t

  (* change eventually to hof *)
  val unify : t -> t -> ('a, t puf) Nuf_func.megapuf -> ('a, t puf) Nuf_func.megapuf list
  val fresh : ('a, t puf) Nuf_func.megapuf -> t * ('a, t puf) Nuf_func.megapuf
  val to_string : t -> string
end

module Letter = struct
  type t = string
  let unify x y p = 
    if x = y then [p]
    else []
  let fresh _ = failwith "todo"
  let to_string = Fun.id
end
