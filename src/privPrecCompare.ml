open! Defaults (* CircInterval / Enums / ... need initialized conf *)
open! Batteries
open Prelude
open Ana

module VD = BaseDomain.VD
(* TODO: share with PrecisionDumpPriv *)
module LVH = Hashtbl.Make (Printable.Prod (Basetype.ProgLines) (Basetype.Variables))

let load filename =
  let f = open_in_bin filename in
  let lvh: VD.t LVH.t = Marshal.from_channel f in
  close_in_noerr f;
  lvh

module Comparison =
struct
  type t =
    | Equal
    | MorePrecise
    | LessPrecise
    | Incomparable

  let aggregate_same c1 c2 = match c1, c2 with
    | Incomparable, _
    | _, Incomparable
    | MorePrecise, LessPrecise
    | LessPrecise, MorePrecise ->
      Incomparable
    | Equal, c
    | c, Equal ->
      c
    | MorePrecise, MorePrecise ->
      MorePrecise
    | LessPrecise, LessPrecise ->
      LessPrecise

  let to_string_infix = function
    | Equal -> "equal to"
    | MorePrecise -> "more precise than"
    | LessPrecise -> "less precise than"
    | Incomparable -> "incomparable to"
end


let compare_dumps filename1 filename2 =
  (* TODO: don't load multiple times *)
  let lvh1 = load filename1 in
  let lvh2 = load filename2 in
  let lvh = LVH.merge (fun k v1 v2 -> Some (v1, v2)) lvh1 lvh2 in
  let compared = LVH.map (fun (l, x) (v1, v2) ->
      let v1 = v1 |? VD.bot () in
      let v2 = v2 |? VD.bot () in
      let c = match VD.leq v1 v2, VD.leq v2 v1 with
        | true, true -> Comparison.Equal
        | true, false -> Comparison.MorePrecise
        | false, true -> Comparison.LessPrecise
        | false, false -> Comparison.Incomparable
      in
      let msg = Pretty.dprintf "%s %s %s (%a %s %a)" filename1 (Comparison.to_string_infix c) filename2 VD.pretty v1 (Comparison.to_string_infix c) VD.pretty v2 in
      (c, msg)
    ) lvh
  in
  LVH.iter (fun (l, x) (c, msg) ->
      match c with
      | Comparison.Equal -> ()
      | _ ->
        ignore (Pretty.printf "%a %a: %t\n" d_loc l d_varinfo x (fun () -> msg))
    ) compared;
  let c = LVH.fold (fun _ (c, _) acc -> Comparison.aggregate_same c acc) compared Comparison.Equal in
  let msg = Pretty.dprintf "%s %s %s" filename1 (Comparison.to_string_infix c) filename2 in
  (c, msg)

let () =
  let filenames = List.tl (Array.to_list Sys.argv) in
  List.cartesian_product filenames filenames
  |> List.filter (fun (filename1, filename2) -> String.compare filename1 filename2 < 0)
  |> List.map (uncurry compare_dumps)
  |> List.iter (fun (_, msg) -> ignore (Pretty.printf "%t\n" (fun () -> msg)))