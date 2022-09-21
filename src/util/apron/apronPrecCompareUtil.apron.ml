open PrecCompareUtil
open ApronDomain

module MyNode =
struct
  include Node
  (* Override the name to "nodes", as plural fits better in the output format of PrePrivPrecCompare *)
  let name () = "nodes"
  let to_location n = Node.location n
end

(* Currently serialization of Apron results only works for octagons. *)
module OctagonD = ApronDomain.OctagonD2
module Util =
struct
  include Util (MyNode) (OctagonD)
  type marshal = OctagonD.marshal RH.t
  type dump = marshal dump_gen
  type result = Dom.t RH.t result_gen

    let init () =
      Apron.Manager.set_deserialize OctagonManager.mgr

    let unmarshal (m: marshal): D2.t RH.t =
      RH.map (fun _ -> D2.unmarshal) m
  end

include Util(OctagonD)
