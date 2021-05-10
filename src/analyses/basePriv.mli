open Cil
open Analyses
(* Cannot use local module substitutions because ppx_import is still stuck at 4.07 AST: https://github.com/ocaml-ppx/ppx_import/issues/50#issuecomment-775817579. *)

type ('d, 'g, 'c) ctx = {
  local: 'd;
  global: varinfo -> 'g;
  sideg: varinfo -> 'g -> unit;
  ask: 'a. 'a Queries.t -> 'a Queries.result;
}

module type S =
sig
  module D: Lattice.S
  module G: Lattice.S

  val startstate: unit -> D.t

  val read_global: (BaseDomain.BaseComponents (D).t, G.t, 'c) ctx -> varinfo -> BaseDomain.VD.t
  val write_global: (BaseDomain.BaseComponents (D).t, G.t, 'c) ctx -> ?invariant:bool -> varinfo -> BaseDomain.VD.t -> BaseDomain.BaseComponents (D).t

  val lock: (BaseDomain.BaseComponents (D).t, G.t, 'c) ctx -> LockDomain.Addr.t -> BaseDomain.BaseComponents (D).t
  val unlock: (BaseDomain.BaseComponents (D).t, G.t, 'c) ctx -> LockDomain.Addr.t -> BaseDomain.BaseComponents (D).t

  val sync: Queries.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> [`Normal | `Join | `Return | `Init | `Thread] -> BaseDomain.BaseComponents (D).t

  val escape: Queries.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> EscapeDomain.EscapedVars.t -> BaseDomain.BaseComponents (D).t
  val enter_multithreaded: Queries.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseDomain.BaseComponents (D).t -> BaseDomain.BaseComponents (D).t
  val threadenter: Queries.ask -> BaseDomain.BaseComponents (D).t -> BaseDomain.BaseComponents (D).t

  val init: unit -> unit
  val finalize: unit -> unit
end

val get_priv : unit -> (module S)
