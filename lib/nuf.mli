open! Batteries

type 'a nuf
type 'a puf

val search : int -> 'a puf -> int * 'a
val search_all : int -> 'a nuf -> (int * 'a) list
val merge_det : ('a -> 'a -> 'a puf -> 'a * 'a puf) -> int -> int -> 'a puf -> 'a puf
val merge : ('a -> 'a -> 'a puf -> ('a * 'a puf) list) -> int -> int -> 'a puf -> 'a nuf
val mergei : (int -> 'a -> int -> 'a -> 'a puf -> ('a * 'a puf) list) -> int -> int -> 'a puf -> 'a nuf
val (<|>) : 'a nuf -> 'a nuf -> 'a nuf
val pure : 'a puf -> 'a nuf
val empty : 'a puf
val set_det : int -> 'a -> 'a puf -> 'a puf
val update_det : int -> ('a -> 'a) -> 'a puf -> 'a puf
val update_nondet : int -> ('a -> 'a list) -> 'a puf -> 'a nuf
val add_det : int -> 'a -> 'a puf -> 'a puf
val add_nondet : int -> 'a -> 'a nuf -> 'a nuf
val (<&>) : 'a nuf -> ('a puf -> 'a puf) -> 'a nuf
val (>>=) : 'a nuf -> ('a puf -> 'a nuf) -> 'a nuf
