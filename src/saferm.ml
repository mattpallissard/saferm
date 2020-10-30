open Lib
open Unix
open Printf


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
      let j = Tree.ISet.insert h d in
      set_tree j t

let printit x = function
  | true -> printf "%d true\n" x
  | false -> printf "%d false\n" x

let () =
  let inodes = Inodes.get_inodes "/proc" in
  let d = set_tree Tree.ISet.empty inodes in
  let f = get_shm "/tmp/test" in
  List.iter (fun (x : int) -> printit x (Tree.ISet.member x d)) f
