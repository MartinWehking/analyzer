open Prelude

let rec mkdir_parents filename =
  let dirname = Filename.dirname filename in
  if not (Sys.file_exists dirname) then (
    mkdir_parents dirname;
    Unix.mkdir dirname 0o770; (* TODO: what permissions? *)
  )

let mkdir_or_exists dirname =
  try
    Unix.mkdir dirname 0o770 (* TODO: what permissions? *)
  with Unix.Unix_error (Unix.EEXIST, _, _) ->
    assert (Sys.is_directory dirname) (* may exist, but as a file *)

let rmdir_if_empty dirname =
  try
    Unix.rmdir dirname
  with Unix.Unix_error (Unix.ENOTEMPTY, _, _) ->
    ()
