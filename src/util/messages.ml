open Cil
open Pretty
open GobConfig
module GU = Goblintutil

type array_oob =
  | PastEnd
  | BeforeStart
  | Unknown
  [@@deriving eq]

type undefined_behavior =
  | ArrayOutOfBounds of array_oob
  | NullPointerDereference
  | UseAfterFree
  [@@deriving eq]

type behavior =
  | Undefined of undefined_behavior
  | Implementation
  | Machine
  [@@deriving eq]

type integer = Overflow | DivByZero [@@deriving eq]

type cast = TypeMismatch [@@deriving eq]

type warning =
  | Behavior of behavior
  | Integer of integer
  | Race
  | Cast of cast
  | Unknown
  | Debug
  | Analyzer
  [@@deriving eq]

module Warning =
struct
  type t = warning [@@deriving eq]

  let hash x = Hashtbl.hash x (* nested variants, so this is fine *)

  module Behavior =
  struct
    type t = behavior

    let create (e: t): warning = Behavior e
    let undefined e: warning = create @@ Undefined e
    let implementation (): warning = create @@ Implementation
    let machine (): warning = create @@ Machine

    module Undefined =
    struct
      type t = undefined_behavior

      let create (e: t): warning = undefined e
      let array_out_of_bounds e: warning = create @@ ArrayOutOfBounds e
      let nullpointer_dereference (): warning = create @@ NullPointerDereference
      let use_after_free (): warning = create @@ UseAfterFree

      module ArrayOutOfBounds =
      struct
        type t = array_oob

        let create (e: t): warning = array_out_of_bounds e
        let past_end (): warning = create PastEnd
        let before_start (): warning = create BeforeStart
        let unknown (): warning = create Unknown

        let from_string_list (s: string list): warning =
          match s with
          | [] -> Unknown
          | h :: t -> match h with
            | "past_end" -> past_end ()
            | "before_start" -> before_start ()
            | "unknown" -> unknown ()
            | _ -> Unknown

        let show (e: t): string =
          match e with
          | PastEnd -> "PastEnd]" ^ " Index is past the end of the array."
          | BeforeStart -> "BeforeStart]" ^ " Index is before start of the array."
          | Unknown -> "Unknown]" ^ " Not enough information about index."
      end

      let from_string_list (s: string list): warning =
        match s with
        | [] -> Unknown
        | h :: t -> match h with
          | "array_out_of_bounds" -> ArrayOutOfBounds.from_string_list t
          | "nullpointer_dereference" -> nullpointer_dereference ()
          | "use_after_free" -> use_after_free ()
          | _ -> Unknown

      let show (e: t): string =
        match e with
        | ArrayOutOfBounds e -> "ArrayOutOfBounds > "^(ArrayOutOfBounds.show e)
        | NullPointerDereference -> "NullPointerDereference]"
        | UseAfterFree -> "UseAfterFree]"
    end

    let from_string_list (s: string list): warning =
      match s with
      | [] -> Unknown
      | h :: t -> ();match h with
        | "undefined" -> Undefined.from_string_list t
        | "implementation" -> implementation ()
        | "machine" -> machine ()
        | _ -> Unknown

    let show (e: t): string =
      match e with
      | Undefined u -> "Undefined > "^(Undefined.show u)
      | Implementation -> "Implementation > "
      | Machine -> "Machine > "
  end

  module Integer =
  struct
    type t = integer

    let create (e: t): warning = Integer e
    let overflow (): warning = create Overflow
    let div_by_zero (): warning = create DivByZero

    let from_string_list (s: string list): warning =
      match s with
      | [] -> Unknown
      | h :: t -> ();match h with
        | "overflow" -> overflow ()
        | "div_by_zero" -> div_by_zero ()
        | _ -> Unknown

    let show (e: t): string =
      match e with
      | Overflow -> "Overflow]"
      | DivByZero -> "DivByZero]"
  end

  module Cast =
  struct
    type t = cast

    let create (e: t): warning = Cast e
    let type_mismatch (): warning = create TypeMismatch

    let from_string_list (s: string list): warning =
      match s with
      | [] -> Unknown
      | h :: t -> ();match h with
        | "type_mismatch" -> type_mismatch ()
        | _ -> Unknown

    let show (e: t): string =
      match e with
      | TypeMismatch -> "TypeMismatch]"
  end

  let should_warn e =
    let to_string e =
      match e with
      | Behavior _ -> "behavior"
      | Integer _ -> "integer"
      | Race -> "race"
      | Cast _ -> "cast"
      | Unknown -> "unknown"
      | Debug -> "debug"
      | Analyzer -> "analyzer"
    in get_bool ("warn." ^ (to_string e))

  let show e =
    match e with
    | Behavior x -> "[Behavior > " ^ (Behavior.show x)
    | Integer x -> "[Integer > " ^ (Integer.show x)
    | Race -> "[Race]"
    | Cast x -> "[Cast > " ^ (Cast.show x)
    | Unknown -> "[Unknown]"
    | Debug -> "[Debug]"
    | Analyzer -> "[Analyzer]"

  let from_string_list (s: string list) =
    match s with
    | [] -> Unknown
    | h :: t -> match h with
      | "behavior" -> Behavior.from_string_list t
      | "integer" -> Integer.from_string_list t
      | "race" -> Race
      | "cast" -> Cast.from_string_list t
      | "debug" -> Debug
      | "analyzer" -> Analyzer
      | _ -> Unknown
end

module Certainty = struct
  type t = May | Must [@@deriving eq]

  let hash x = Hashtbl.hash x (* variants, so this is fine *)

  let should_warn e =
    let to_string e =
      match e with
      | May -> "may"
      | Must -> "must"
    in get_bool ("warn." ^ (to_string e))

  let show c =
    match c with
    | May -> "[May]"
    | Must -> "[Must]"
end

module WarningWithCertainty =
struct
  type t = {
    warn_type : Warning.t;
    certainty: Certainty.t option
  }

  let should_warn (e:t) = Warning.should_warn e.warn_type && (match e.certainty with Some c -> Certainty.should_warn c | _ -> true)

  let debug () = {warn_type = Debug; certainty = None}

  let create ?must:(must=false) w = {warn_type = w; certainty = Some (if must then Certainty.Must else Certainty.May)}
  let show {warn_type; certainty} =
    let certainty_str = match certainty with
      | Some c -> (Certainty.show c)
      | None -> ""
    and warning_tag = match warn_type with
      | Debug -> ""
      | _ -> "[Warning]"
    in warning_tag^certainty_str^(Warning.show warn_type)
end

module Message =
struct
  type t = {
    warn_type: Warning.t; (* TODO: make list of tags *)
    certainty: Certainty.t option; (* TODO: change to severity levels, make non-option *)
    loc: CilType.Location.t option; (* only *_each warnings have this, used for deduplication *)
    text: string;
    context: (Obj.t [@equal fun x y -> Hashtbl.hash (Obj.obj x) = Hashtbl.hash (Obj.obj y)]) option; (* TODO: this equality is terrible... *)
    print_loc: CilType.Location.t [@equal fun _ _ -> true]; (* all warnings have this, not used for deduplication *)
  } [@@deriving eq]

  let hash {warn_type; certainty; loc; text; context; print_loc} =
    3 * Warning.hash warn_type + 5 * BatOption.map_default Certainty.hash 1 certainty + 7 * BatOption.map_default CilType.Location.hash 1 loc + 9 * Hashtbl.hash text + 11 * BatOption.map_default (fun c -> Hashtbl.hash (Obj.obj c)) 1 context

  let with_context msg = function
    | Some ctx when GobConfig.get_bool "dbg.warn_with_context" -> msg ^ " in context " ^ string_of_int (Hashtbl.hash ctx) (* TODO: this is kind of useless *)
    | _ -> msg

  let show {warn_type; certainty; loc; text; context; print_loc} =
    let text = match warn_type with
      | Debug -> "{BLUE}"^text (* TODO: don't do it like this *)
      | _ -> text
    in
    let msg = (WarningWithCertainty.(show {warn_type; certainty}))^(if text != "" then " "^text else "") in
    let msg = with_context msg context in
    msg
end

module MH = Hashtbl.Make (Message)
let messages_table = MH.create 113 (* messages without order for quick mem lookup *)
let messages_list = ref [] (* messages with reverse order (for cons efficiency) *)


exception Bailure of string
let bailwith s = raise (Bailure s)

let warning_table : [`text of string * location | `group of string * ((string * location) list)] list ref = ref []
let warn_out = ref stdout
let tracing = Config.tracing
let xml_file_name = ref ""

let push_warning w =
  warning_table := w :: !warning_table


(*Warning files*)
let warn_race = ref stdout
let warn_safe = ref stdout
let warn_higr = ref stdout
let warn_higw = ref stdout
let warn_lowr = ref stdout
let warn_loww = ref stdout

let init_warn_files () =
  warn_race := (open_out "goblint_warnings_race.txt");
  warn_safe := (open_out "goblint_warnings_safe.txt");
  warn_higr := (open_out "goblint_warnings_highreadrace.txt");
  warn_higw := (open_out "goblint_warnings_highwriterace.txt");
  warn_lowr := (open_out "goblint_warnings_lowreadrace.txt");
  warn_loww := (open_out "goblint_warnings_lowwriterace.txt")

let get_out name alternative = match get_string "dbg.dump" with
  | "" -> alternative
  | path -> open_out (Filename.concat path (name ^ ".out"))

let colors_on () = (* use colors? *)
  let c = get_string "colors" in
  c = "always" || c = "auto" && Unix.(isatty stdout)

let colorize ?on:(on=colors_on ()) msg =
  let colors = [("gray", "30"); ("red", "31"); ("green", "32"); ("yellow", "33"); ("blue", "34");
                ("violet", "35"); ("turquoise", "36"); ("white", "37"); ("reset", "0;00")] in
  let replace (color,code) =
    let modes = [(fun x -> x), "0" (* normal *); String.uppercase_ascii, "1" (* bold *)] in
    List.fold_right (fun (f,m) -> Str.global_replace (Str.regexp ("{"^f color^"}")) (if on then "\027["^m^";"^code^"m" else "")) modes
  in
  let msg = List.fold_right replace colors msg in
  msg^(if on then "\027[0;0;00m" else "") (* reset at end *)

let print_msg msg loc =
  let msgc = colorize msg in
  let msg  = colorize ~on:false msg in
  push_warning (`text (msg, loc));
  let color = if colors_on () then "{violet}" else "" in
  let s = Printf.sprintf "%s %s(%s)" msgc color (CilType.Location.show loc) in
  Printf.fprintf !warn_out "%s\n%!" (colorize s)


let print_group group_name errors =
  (* Add warnings to global warning list *)
  push_warning (`group (group_name, errors));
  let f (msg,loc): doc = Pretty.dprintf "%s (%a)" msg CilType.Location.pretty loc in
  if (get_bool "ana.osek.warnfiles") then begin
    match (String.sub group_name 0 6) with
    | "Safely" -> ignore (Pretty.fprintf !warn_safe "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "Datara" -> ignore (Pretty.fprintf !warn_race "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "High r" -> ignore (Pretty.fprintf !warn_higr "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "High w" -> ignore (Pretty.fprintf !warn_higw "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "Low re" -> ignore (Pretty.fprintf !warn_lowr "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | "Low wr" -> ignore (Pretty.fprintf !warn_loww "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)
    | _ -> ()
  end;
  ignore (Pretty.fprintf !warn_out "%s:\n  @[%a@]\n" group_name (docList ~sep:line f) errors)


let warn_all m =
  if !GU.should_warn then (
    let wc = WarningWithCertainty.{warn_type = m.Message.warn_type; certainty = m.Message.certainty} in (* TODO: don't reconstruct this *)
    if WarningWithCertainty.should_warn wc && not (MH.mem messages_table m) then (
      print_msg (Message.show m) m.print_loc;
      MH.replace messages_table m ();
      messages_list := m :: !messages_list
    )
  )

let current_context: Obj.t option ref = ref None (** (Control.get_spec ()) context, represented type: (Control.get_spec ()).C.t *)

let warn_internal ?msg:(msg="") (warning: WarningWithCertainty.t) =
  warn_all {warn_type = warning.warn_type; certainty = warning.certainty; loc = None; text = msg; context = !current_context; print_loc = !Tracing.current_loc}

let warn_internal_with_loc ?loc:(loc= !Tracing.current_loc) ?msg:(msg="") (warning: WarningWithCertainty.t) =
  warn_all {warn_type = warning.warn_type; certainty = warning.certainty; loc = Some loc; text = msg; context = !current_context; print_loc = loc}

let warn ?must:(must=false) ?msg:(msg="") ?warning:(warning=Unknown) () =
  warn_internal ~msg:msg (WarningWithCertainty.create ~must:must warning)

let warn_each ?must:(must=false) ?loc ?msg:(msg="") ?warning:(warning=Unknown) () =
  warn_internal_with_loc ?loc ~msg:msg (WarningWithCertainty.create ~must:must warning)

let debug msg =
  warn_internal ~msg @@ WarningWithCertainty.debug ()

let debug_each msg =
  warn_internal_with_loc ~msg @@ WarningWithCertainty.debug ()

include Tracing
