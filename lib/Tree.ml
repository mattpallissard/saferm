open Printf

type comparison = Lt | Eq | Gt

module type ORDERED =
  sig
    type t
      val compare : t -> t -> comparison
      val print: t -> unit
  end


module type SET =
  sig
    type elem
    type set
    val empty : set
    val insert : elem -> set -> set
    val member : elem -> set -> bool
    val display : set -> unit
  end

(** binary tree from ch2 of Okasaki's book *)
module UnbalancedSet (Element : ORDERED) : (SET with type elem = Element.t) =
struct
  type elem = Element.t
  type set =
    | E
    | T of set * elem * set

  let empty = E

  let rec member x = function
    | E -> false
    | T (l, y, r) ->
        match Element.compare x y with
          | Eq -> true
          | Lt -> member x l
          | Gt -> member x r

    exception EXISTS
    let insert x = function
      | E -> T(E, x, E)
      | s -> let one = function
          | E -> T(E, x, E)
          | T(_, m, _)  ->
            let rec two m = function
              | E -> (match Element.compare x m with
                  | Eq -> raise EXISTS
                  | _  -> T(E, x, E))
              | T(l, m', r) -> (match Element.compare x m' with
                  | Lt -> T(two m l, m', r)
                  | _  -> T(l, m', two m r))
            in try two m s with EXISTS -> s
        in one s

  let rec display = function
    | E -> ()
    | T (l, y, r) ->
        Element.print (y);
        display l; display r;
end


module OrderedInodes=
  struct
    type t = Unix.file_perm
    let compare i j =
      if i = j then Eq
      else if i < j then Lt
      else Gt
    let print i  =  printf "%d\n" i
  end


module ISet =  UnbalancedSet(OrderedInodes)
