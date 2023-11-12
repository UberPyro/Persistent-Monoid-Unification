open! Batteries

module M = Ulist.Make(U.Letter)

let m : (string, int) Hashtbl.t = Hashtbl.create 32
let memo x = 
  Hashtbl.find_option m x
  |> Option.default_delayed @@ fun () -> 
    let nu = unique () in
    Hashtbl.add m x nu;
    nu
