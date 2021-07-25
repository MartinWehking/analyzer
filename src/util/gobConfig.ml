(**
   New, untyped, path-based configuration subsystem.

   {v
  path' ::== \epsilon              (*  *)
           | . <field-name> path'  (* field access *)
           | [ <index-nr> ] path'  (* array index access *)
           | [ + ] path'           (* cons to array *)
           | [ - ] path'           (* cons away from array *)
           | [ * ] path'           (* reset array *)

  path ::==              path'     (*  *)
          | <field_name> path'     (* you can leave out the first dot *)
  v}

   All functions [failwith] on error. Warnings are generated in [verbose] mode.

   There is a "conf" [trace] option that traces setting.
*)

open Prelude
open Tracing
open Config
open Printf
open JsonSchema
open Json

exception ConfigError of string

let build_config = ref false

(* Phase of the analysis (moved from GoblintUtil b/c of circular build...) *)
let phase = ref 0
let phase_config = ref true

(** The type for [gobConfig] module. *)
module type S =
sig
  (** Functions to query conf variable of type int. *)
  val get_int    : string -> int

  (** Functions to modify conf variables of type int. *)
  val set_int    : string -> int    -> unit

  (** Functions to query conf variable of type bool. *)
  val get_bool   : string -> bool

  (** Functions to modify conf variables of type bool. *)
  val set_bool   : string -> bool   -> unit

  (** Functions to query conf variable of type string. *)
  val get_string : string -> string

  (** Functions to modify conf variables of type string. *)
  val set_string : string -> string -> unit

  (** Functions to modify conf variables by trying to parse the value.
      The second argument must be valid Json except single quotes represent double quotes. *)
  val set_auto   : string -> string -> unit

  (** Get a list of values *)
  val get_list : string -> jvalue list

  (** Get a list of strings *)
  val get_string_list : string -> string list

  (** Set a list of values *)
  val set_list : string -> jvalue list -> unit

  (** Functions to set a conf variables to null. *)
  val set_null   : string -> unit

  (** Functions to modify conf array variables to drop one index. *)
  val drop_index : string -> int    -> unit

  (** Print the current configuration *)
  val print : 'a BatInnerIO.output -> unit

  (** Write the current configuration to [filename] *)
  val write_file: string -> unit

  (** Merge configurations form a file with current. *)
  val merge_file : string -> unit

  (** Add a schema to the conf*)
  val addenum_sch: jvalue -> unit

  (** Whether for a given ikind, we should compute with wrap-around arithmetic.
    *  Always for unsigned types, for signed types if 'sem.int.signed_overflow' is 'assume_wraparound'  *)
  val should_wrap: Cil.ikind -> bool

  (** Whether for a given ikind, we should assume there are no overflows.
    * Always false for unsigned types, true for signed types if 'sem.int.signed_overflow' is 'assume_none'  *)
  val should_ignore_overflow: Cil.ikind -> bool
end

(** The implementation of the [gobConfig] module. *)
module Impl : S =
struct
  (** raise when you cannot parse the path *)
  exception PathParseError

  (** raise when there is an type error *)
  exception ConfTypeError

  (** Type of the index *)
  type index = Int of int  (** and integer *)
             | App         (** prepend to the list *)
             | Rem         (** remove from the list *)
             | New         (** create a new list *)

  (** Type of the path *)
  type path  = Here                    (** we are there *)
             | Select of string * path (** we need to select an field *)
             | Index  of index  * path (** we need to select an array index *)

  (** Path printing. *)
  let rec print_path' ch = function
    | Here -> ()
    | Select (s,p)    -> fprintf ch ".%s%a"  s print_path' p
    | Index (Int i,p) -> fprintf ch "[%d]%a" i print_path' p
    | Index (App ,p) -> fprintf ch "[+]%a"    print_path' p
    | Index (Rem ,p) -> fprintf ch "[-]%a"    print_path' p
    | Index (New  ,p) -> fprintf ch "[*]%a"    print_path' p

  (** Path printing where you can ignore the first dot. *)
  let print_path ch = function
    | Select (s,p) -> fprintf ch "%s%a" s print_path' p
    | pth -> print_path' ch pth

  (** Helper function [split c1 c2 xs] that splits [xs] on [c1] or [c2] *)
  let split c1 c2 xs =
    let l = String.length xs in
    let rec split' i =
      if i<l then begin
        if xs.[i]=c1 || xs.[i]=c2 then
          (String.sub xs 0 i, String.sub xs i (l-i))
        else
          split' (i+1)
      end else
        (xs,"")
    in
    split' 0

  (** Parse an index. *)
  let parse_index s =
    try if s = "+" then App
      else if s = "*" then New
      else if s = "-" then Rem
      else Int (int_of_string s)
    with Failure _ -> raise PathParseError

  (** Parse a string path. *)
  let rec parse_path' (s:string) : path =
    if String.length s = 0 then Here else
      match s.[0] with
      | '.' ->
        let fld, pth = split '.' '[' (String.lchop s) in
        Select (fld, parse_path' pth)
      | '[' ->
        let idx, pth = String.split (String.lchop s) "]" in
        Index (parse_index idx, parse_path' pth)
      | _ -> raise PathParseError

  (** Parse a string path, but you may ignore the first dot. *)
  let parse_path (s:string) : path =
    let s = String.trim s in
    try
      if String.length s = 0 then Here else begin
        let fld, pth = split '.' '[' s in
        if fld = ""
        then parse_path' pth
        else Select (fld, parse_path' pth)
      end
    with PathParseError ->
      eprintf "Error: Couldn't parse the json path '%s'\n%!" s;
      failwith "parsing"

  (** Here we store the actual configuration. *)
  let json_conf : jvalue ref = ref Null

  (** The schema for the conf [json_conf] *)
  let conf_schema : jschema =
    { sid      = Some "root"
    ; sdescr   = Some "Configuration root for the Goblint."
    ; stype    = None
    ; sdefault = None
    ; saddenum = []
    }

  (** Add the schema to [conf_schema]. *)
  let addenum_sch jv = addenum conf_schema @@ fromJson jv

  (** Helper function to print the conf using [printf "%t"] and alike. *)
  let print ch : unit =
    printJson ch !json_conf
  let write_file filename = File.with_file_out filename print

  (** Main function to receive values from the conf. *)
  let rec get_value o pth =
    match o, pth with
    | o, Here -> o
    | Object m, Select (key,pth) -> begin
        try get_value !(Object.find key !m) pth
        with Not_found -> raise ConfTypeError end
    | Array a, Index (Int i, pth) -> get_value !(List.at !a i) pth
    | _ -> raise ConfTypeError

  (** Recursively create the value for some new path. *)
  let rec create_new v = function
    | Here -> v
    | Select (key,pth) -> Build.objekt [key,create_new v pth]
    | Index (_, pth) -> Build.array [create_new v pth]

  (** Helper function to decide if types in the json conf have changed. *)
  let json_type_equals x y =
    match x, y with
    | String _, String _
    | Number _, Number _
    | Object _, Object _
    | Array  _, Array  _
    | True    , True
    | False   , False
    | False   , True
    | True    , False
    | Null    , Null     -> true
    | _                  -> false

  (** The main function to write new values into the conf. *)
  let set_value v o orig_pth =
    let rec set_value v o pth =
      match !o, pth with
      | Object m, Select (key,pth) ->
        begin try set_value v (Object.find key !m) pth
          with Not_found ->
            if !build_config then
              m := Object.add key (ref (create_new v pth)) !m
            else
              raise @@ ConfigError ("Unknown path "^ (sprintf2 "%a" print_path orig_pth))
        end
      | Array a, Index (Int i, pth) ->
        set_value v (List.at !a i) pth
      | Array a, Index (App, pth) ->
        o := Array (ref (!a @ [ref (create_new v pth)]))
      | Array a, Index (Rem, pth) ->
        let original_list = !a in
        let excluded_elem = create_new v pth in
        let filtered_list =
          List.filter (fun ref_elem ->
            let elem = !ref_elem in
            match (elem, excluded_elem) with
            | (String s1, String s2) -> not (String.equal s1 s2)
            | (_, _) -> failwith "At the moment it's only possible to remove a string from an array."
            ) original_list in
        o := Array (ref filtered_list)
      | Array _, Index (New, pth) ->
        o := Array (ref [ref (create_new v pth)])
      | Null, _ ->
        o := create_new v pth
      | _ ->
        let new_v = create_new v pth in
        if not (json_type_equals !o new_v) then
          printf "Warning, changing '%a' from '%a' to '%a'.\n"
            print_path orig_pth printJson !o printJson new_v;
        o := new_v
    in
    set_value v o orig_pth;
    validate conf_schema !json_conf

  (** Helper function for reading values. Handles error messages. *)
  let get_path_string f st =
    try
      let st = String.trim st in
      let st, x =
        let g st = st, get_value !json_conf (parse_path st) in
        if !phase_config then
          try g ("phases["^ string_of_int !phase ^"]."^st) (* try to find value in config for current phase first *)
          with _ -> g st (* do global lookup if undefined *)
        else
          g st (* just use the old format *)
      in
      if tracing then trace "conf-reads" "Reading '%s', it is %a.\n" st prettyJson x;
      try f x
      with JsonE s ->
        eprintf "The value for '%s' has the wrong type: %s\n" st s;
        failwith "get_path_string"
    with ConfTypeError ->
      eprintf "Cannot find value '%s' in\n%t\nDid You forget to add default values to defaults.ml?\n"
        st print;
      failwith "get_path_string"

  (** Convenience functions for reading values. *)
  (* memoize for each type with BatCache: *)
  let memo gen = BatCache.make_ht ~gen ~init_size:5 (* uses hashtable; fine since our options are bounded *)
  let memog f = memo @@ get_path_string f

  let memo_int    = memog number
  let memo_bool   = memog bool
  let memo_string = memog string
  let memo_list   = memo @@ List.map (!) % (!) % get_path_string array

  let drop_memo ()  =
    (* The explicit polymorphism is needed to make it compile *)
    let drop:'a. (string,'a) BatCache.manual_cache -> _ = fun m ->
      let r = m.enum () in
      BatEnum.force r; BatEnum.iter (fun (k,v) -> m.del k) r
    in
    drop memo_int; drop memo_bool; drop memo_string; drop memo_list

  let get_int    = memo_int.get
  let get_bool   = memo_bool.get
  let get_string = memo_string.get
  let get_list   = memo_list.get
  let get_string_list = List.map string % get_list

  (** Helper functions for writing values. *)
  let set_path_string st v =
    set_value v json_conf (parse_path st)

  (** Helper functions for writing values. Handels the tracing. *)
  let set_path_string_trace st v =
    if not !build_config then drop_memo ();
    if tracing then trace "conf" "Setting '%s' to %a.\n" st prettyJson v;
    set_path_string st v

  (** Convenience functions for writing values. *)
  let set_int    st i = set_path_string_trace st (Build.number i)
  let set_bool   st i = set_path_string_trace st (Build.bool i)
  let set_string st i = set_path_string_trace st (Build.string i)
  let set_null   st   = set_path_string_trace st Build.null
  let set_list   st l = set_value (Build.array l) json_conf (parse_path st)

  (** A convenience functions for writing values. *)
  let set_auto' st v =
    if v = "null" then set_null st else
      try set_bool st (bool_of_string v)
      with Invalid_argument _ ->
      try set_int st (int_of_string v)
      with Failure _ ->
        set_string st v

  (** The ultimate convenience function for writing values. *)
  let one_quote = Str.regexp "\'"
  let set_auto st s =
    if s="null" then set_null st else
    if s="" then set_string st "" else
      try
        let s' = Str.global_replace one_quote "\"" s in
        let v = JsonParser.value JsonLexer.token (Lexing.from_string s') in
        set_path_string_trace st v
      with e ->
        eprintf "Cannot set %s to '%s'.\n" st s;
        raise e

  (** Merge configurations form a file with current. *)
  let merge_file fn =
    let v = JsonParser.value JsonLexer.token % Lexing.from_channel |> File.with_file_in fn in
    json_conf := merge !json_conf v;
    drop_memo ();
    if tracing then trace "conf" "Merging with '%s', resulting\n%a.\n" fn prettyJson !json_conf

  (** Function to drop one element of an 'array' *)
  let drop_index st i =
    let old = get_path_string array st in
    if tracing then
      trace "conf" "Removing index %d from '%s' to %a." i st prettyJson (Array old);
    match List.split_at i !old with
    | pre, _::post -> set_path_string st (Array (ref (pre@post)))
    | _ ->
      eprintf "Cannot drop index %d in array %s:\n%t\n\n" i st print;
      failwith "drop_index"

  let should_wrap ik = not (Cil.isSigned ik) || get_string "sem.int.signed_overflow" = "assume_wraparound"

  let should_ignore_overflow ik = Cil.isSigned ik && get_string "sem.int.signed_overflow" = "assume_none"
end

include Impl
