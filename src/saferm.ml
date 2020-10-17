open Str
open Printf
open Unix

type comparison = Lt | Eq | Gt

module type ORDERED =
  sig
    type t
      val compare : t -> t -> comparison
      val print: t -> unit
  end

module OrderedInt =
  struct
    type t = file_perm
    let compare i j =
      if i = j then Eq
      else if i < j then Lt
      else Gt
    let print i  =  printf "%d\n" i
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

module ISet =  UnbalancedSet(OrderedInt)

let r = Str.regexp "^[0-9]+$"
exception FAIL
let rec get_inodes path =
    let fd = opendir path in
    let rec aux items =
      try
        let dir = readdir fd in
        let file = path ^ "/" ^ dir in
        match string_match r dir 0 with
          | true ->
              (try
                match (lstat file).st_kind with
                | S_DIR -> get_inodes file @ aux items
                | S_LNK -> (stat file).st_ino :: aux items
                | _ -> raise FAIL (* should never get here *)
              with
               | Unix_error(_, _, _) -> aux items) (* proc is a moving target, continue *)
          | false ->
            match dir with
            | "fd"  -> get_inodes file @ items
            | "task" -> get_inodes file @ aux items
            | _ -> aux items
      with | End_of_file  -> items
  in
  let i = aux [] in
  closedir fd;
  i

let rec get_shm path =
  let fd = opendir path in
  let rec aux items =
    try
      let file = readdir fd in
      match file with
        | "." | ".." -> aux items
        | _ -> (
          let f = path ^ "/" ^ file in
          match (stat f).st_kind with
          | S_DIR -> get_shm f @ aux items
          | _ -> (lstat f).st_ino :: aux items)
    with
      | Unix_error(_, _, _) -> aux items (* we should report the different types errors here *)
      | End_of_file -> items
  in
  let i = aux [] in
  closedir fd;
  i

let rec set_tree d = function
  | [] -> d
  | h :: t ->
      let j = ISet.insert h d in
      set_tree j t

let printit x = function
  | true -> printf "%d true\n" x
  | false -> printf "%d false\n" x

let () =
  let inodes = get_inodes "/proc" in
  let d = set_tree ISet.empty inodes in
  let f = get_shm "/tmp/test" in
  List.iter (fun (x : int) -> printit x (ISet.member x d)) f
