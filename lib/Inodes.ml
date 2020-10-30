open Str
open Unix

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
