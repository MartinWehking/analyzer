open! Defaults (* CircInterval / Enums / ... need initialized conf *)
open! Batteries
open Prelude
open Ana
open PrivPrecCompareUtil

module VD = BaseDomain.VD

let load filename =
  let f = open_in_bin filename in
  let dump: dump = Marshal.from_channel f in
  close_in_noerr f;
  dump

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


let compare_dumps {name = name1; lvh = lvh1} {name = name2; lvh = lvh2} =
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
      let diff () =
        (if VD.leq v1 v2 then nil else dprintf "diff: %a\n" VD.pretty_diff (v1, v2))
        ++
        (if VD.leq v2 v1 then nil else dprintf "reverse diff: %a\n" VD.pretty_diff (v2, v1))
      in
      let msg = Pretty.dprintf "%s %s %s\n  @[%s: %a\n%s\n%s: %a\n%t@]" name1 (Comparison.to_string_infix c) name2 name1 VD.pretty v1 (Comparison.to_string_infix c) name2 VD.pretty v2 diff in
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
  let msg = Pretty.dprintf "%s %s %s" name1 (Comparison.to_string_infix c) name2 in
  (c, msg)

let () =
  Cil.initCIL (); (* ValueDomain.Compound.leq depends on ptrdiffType initialization *)
  let filenames = List.tl (Array.to_list Sys.argv) in
  let dumps = List.map load filenames in
  let i_dumps = List.mapi (fun i dump -> (i, dump)) dumps in
  List.cartesian_product i_dumps i_dumps
  |> List.filter (fun ((i1, _), (i2, _)) -> i1 < i2)
  |> List.map (Tuple2.map snd snd)
  |> List.map (uncurry compare_dumps)
  |> List.iter (fun (_, msg) -> ignore (Pretty.printf "%t\n" (fun () -> msg)))