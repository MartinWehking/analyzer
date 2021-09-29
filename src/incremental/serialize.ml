open Prelude

let base_directory = ref (Sys.getcwd ()) (* base directory where incremental results are stored *)
let goblint_dirname = "incremental_data"
let version_map_filename = "version.data"
let cil_file_name = "ast.data"
let solver_data_file_name = "solver.data"
let results_dir = "results"
let results_tmp_dir = "results_tmp"
let gob_directory () = let src_dir = !base_directory in
  Filename.concat src_dir goblint_dirname

let gob_results_dir () =
  Filename.concat (gob_directory ()) results_dir

let gob_results_tmp_dir () =
  Filename.concat (gob_directory ()) results_tmp_dir

let marshal obj fileName  =
  let chan = open_out_bin fileName in
  Marshal.output chan obj;
  close_out chan

let unmarshal fileName =
  if GobConfig.get_bool "dbg.verbose" then print_endline ("Unmarshalling " ^ fileName ^ "... If type of content changed, this will result in a segmentation fault!");
  Marshal.input (open_in_bin fileName)

let results_exist () =
  (* If Goblint did not crash irregularly, the existance of the result directory indicates that there are results *)
  let r = gob_results_dir () in
  Sys.file_exists r && Sys.is_directory r

(* Convenience enumeration of the different data types we store for incremental analysis, so file-name logic is concentrated in one place *)
type incremental_data_kind = SolverData | CilFile | VersionData

let type_to_file_name = function
  | SolverData -> solver_data_file_name
  | CilFile -> cil_file_name
  | VersionData -> version_map_filename

(** Loads data for incremntal runs from the appropriate file *)
let load_data (data_type: incremental_data_kind) =
  let p = Filename.concat (gob_results_dir ()) (type_to_file_name data_type) in
  unmarshal p

(** Stores data for future incremental runs at the appropriate file, given the data and what kind of data it is. *)
let store_data (data: 'a) (data_type: incremental_data_kind) =
  ignore @@ Goblintutil.create_dir (gob_directory ());
  let d = gob_results_tmp_dir () in
  ignore @@ Goblintutil.create_dir d;
  let p = Filename.concat (d) (type_to_file_name data_type) in
  marshal data p

(** Deletes previous analysis results and moves the freshly created results there.*)
let move_tmp_results_to_results () =
  if Sys.file_exists (gob_results_dir ()) then begin
    Goblintutil.rm_rf (gob_results_dir ());
  end;
  Sys.rename (gob_results_tmp_dir ()) (gob_results_dir ())
