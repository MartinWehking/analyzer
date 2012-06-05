module GU = Goblintutil

module type S =
sig
  include Printable.S
  val leq: t -> t -> bool
  val join: t -> t -> [ `Left | `Right | `Equal | `New of t]
  val oldjoin: t -> t -> t
  val meet: t -> t -> t
  val bot: unit -> t
  val is_bot: t -> bool
  val top: unit -> t
  val is_top: t -> bool
  val widen: t -> t -> t
  val narrow: t -> t -> t
end

module StdCousot = 
struct
  let widen x y = y
  let narrow x y = x
end

exception TopValue
exception BotValue
exception Unsupported of string
let unsupported x = raise (Unsupported x)

module Unit = 
struct
  include Printable.Unit
  include StdCousot
  let leq _ _ = true
  let join _ _ = `Equal
  let oldjoin _ _ = ()
  let meet _ _ = ()
  let top () = ()
  let is_top _ = true
  let bot () = ()
  let is_bot _ = true
end



module Fake (Base: Printable.S) = 
struct 
  include Base
  include StdCousot
  let leq = equal
  let join x y = 
    if equal x y then `Equal else raise (Unsupported "fake join")
  let oldjoin x y =
    if equal x y then x else raise (Unsupported "fake join")
  let meet x y = 
    if equal x y then x else raise (Unsupported "fake meet")
  let top () = raise (Unsupported "fake top")
  let is_top _ = false
  let bot () = raise (Unsupported "fake bot")
  let is_bot _ = false
end

module type PD = 
sig
  include Printable.S
  val dummy: t
end

module UnsafeFake (Base: PD) =
struct
  include Fake (Base)
  let oldjoin x y = x
  let join x y = `Left
  let meet x y = x
  let top () = Base.dummy
  let bot () = Base.dummy
  let is_top _ = false 
  let is_bot _ = false
end

module Reverse (Base: S) =
struct
  include Base
  include StdCousot (* this isn't good *)  
  let bot = Base.top
  let top = Base.bot
  let leq x y = Base.leq y x
  let join x y = 
    if Base.equal x y then `Equal else
    let l = Base.meet x y in 
    if Base.equal x l then `Left else if Base.equal y l then `Right else `New l
  let oldjoin x y = Base.meet x y
  let meet x y = match Base.join x y with `Left | `Equal -> x | `Right -> y | `New y -> y
end

(* HAS SIDE-EFFECTS ---- PLEASE INSTANCIATE ONLY ONCE!!! *)
module HConsed (Base:S) =
struct
  include Printable.HConsed (Base)
  let lift_f2 f x y = f (unlift x) (unlift y)
  let narrow x y = lift (lift_f2 Base.narrow x y)
  let widen x y = lift (lift_f2 Base.widen x y)
  let meet x y = lift (lift_f2 Base.meet x y)
  let oldjoin x y = lift (lift_f2 Base.oldjoin x y)
  let join x y = match lift_f2 Base.join x y with
                     `New w -> `New (lift w) 
                   | `Left  -> `Left 
                   | `Right -> `Right
                   | `Equal -> `Equal
(* let join x y = 
   let d = oldjoin x y in
   match join x y with
     | `Equal -> assert (equal x y && equal d x); `Equal
     | `Left  -> assert (equal x d); `Left
     | `Right -> assert (equal y d); `Right
     | `New q -> assert (equal d q); `New q *)
  let leq = lift_f2 Base.leq 
  let is_top = lift_f Base.is_top 
  let is_bot = lift_f Base.is_bot 
  let top () = lift (Base.top ())
  let bot () = lift (Base.bot ())
end

module Flat (Base: Printable.S) (N: Printable.LiftingNames) = 
struct 
  include Printable.Lift (Base) (N)
  include StdCousot

  let bot () = `Bot
  let is_bot x = x = `Bot
  let top () = `Top
  let is_top x = x = `Top

  let leq (x:t) (y:t) =
    match (x,y) with
      | (_, `Top) -> true
      | (`Top, _) -> false
      | (`Bot, _) -> true
      | (_, `Bot) -> false
      | (`Lifted x, `Lifted y) -> Base.equal x y

  let pretty_diff () ((x:t),(y:t)): Pretty.doc = 
    if leq x y then Pretty.text "No Changes" else
    Pretty.dprintf "%a instead of %a" pretty x pretty y

  let oldjoin x y = 
    match (x,y) with 
      | (`Top, _) -> `Top
      | (_, `Top) -> `Top
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Lifted x, `Lifted y) when Base.equal x y -> `Lifted x
      | _ -> `Top

  let join x y = 
    match (x,y) with 
      | (`Top, `Top) -> `Equal
      | (`Top, _) -> `Left
      | (_, `Top) -> `Right
      | (`Bot, `Bot) -> `Equal
      | (`Bot, x) -> `Right
      | (x, `Bot) -> `Left
      | (`Lifted x, `Lifted y) when Base.equal x y -> `Equal
      | _ -> `New `Top
      (*
  let join x y = 
    let d = oldjoin x y in
    match join x y with
      | `Equal -> assert (equal x y && equal d x); `Equal
      | `Left  -> assert (equal x d); `Left
      | `Right -> assert (equal y d); `Right
      | `New q -> assert (equal d q); `New q 
      *)
  let meet x y = 
    match (x,y) with 
      | (`Bot, _) -> `Bot
      | (_, `Bot) -> `Bot
      | (`Top, x) -> x
      | (x, `Top) -> x
      | (`Lifted x, `Lifted y) when Base.equal x y -> `Lifted x
      | _ -> `Bot
end


module Lift (Base: S) (N: Printable.LiftingNames) = 
struct 
  include Printable.Lift (Base) (N)

  let bot () = `Bot
  let is_bot x = x = `Bot
  let top () = `Top
  let is_top x = x = `Top

  let leq x y =
    match (x,y) with
      | (_, `Top) -> true
      | (`Top, _) -> false
      | (`Bot, _) -> true
      | (_, `Bot) -> false
      | (`Lifted x, `Lifted y) -> Base.leq x y

  let pretty_diff () ((x:t),(y:t)): Pretty.doc = 
    match (x,y) with
      | (`Lifted x, `Lifted y) -> Base.pretty_diff () (x,y)
      | _ -> if leq x y then Pretty.text "No Changes" else
             Pretty.dprintf "%a instead of %a" pretty x pretty y

  let oldjoin x y = 
    match (x,y) with 
      | (`Top, _) -> `Top
      | (_, `Top) -> `Top
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Lifted x, `Lifted y) -> `Lifted (Base.oldjoin x y)
  
  let join x y = 
    match (x,y) with 
      | (`Top, `Top) -> `Equal
      | (`Top, _) -> `Left
      | (_, `Top) -> `Right
      | (`Bot, `Bot) -> `Equal
      | (`Bot, x) -> `Right
      | (x, `Bot) -> `Left
      | (`Lifted x, `Lifted y) -> 
    match Base.join x y with
      | `Equal -> `Equal
      | `Left  -> `Left
      | `Right -> `Right
      | `New t -> `New (`Lifted t)
      (*
  let join x y = 
    let d = oldjoin x y in
    match join x y with
      | `Equal -> assert (equal x y && equal d x); `Equal
      | `Left  -> assert (equal x d); `Left
      | `Right -> assert (equal y d); `Right
      | `New q -> (if not (equal q d) then ignore (Pretty.printf "joining:\n%a\nand\n%a\nto\n%a\n" pretty x pretty y pretty d)); assert(equal q d); `New q 
      *)
  let meet x y = 
    match (x,y) with 
      | (`Bot, _) -> `Bot
      | (_, `Bot) -> `Bot
      | (`Top, x) -> x
      | (x, `Top) -> x
      | (`Lifted x, `Lifted y) -> `Lifted (Base.meet x y)
      
  let widen x y = 
    match (x,y) with 
      | (`Lifted x, `Lifted y) -> `Lifted (Base.widen x y)
      | _ -> y

  let narrow x y = 
    match (x,y) with 
      | (`Lifted x, `Lifted y) -> `Lifted (Base.narrow x y)
      | _ -> x
end

module Lift2 (Base1: S) (Base2: S) (N: Printable.LiftingNames) = 
struct 
  include Printable.Lift2 (Base1) (Base2) (N)

  let bot () = `Bot
  let is_bot x = x = `Bot
  let top () = `Top
  let is_top x = x = `Top

  let leq x y =
    match (x,y) with
      | (_, `Top) -> true
      | (`Top, _) -> false
      | (`Bot, _) -> true
      | (_, `Bot) -> false
      | (`Lifted1 x, `Lifted1 y) -> Base1.leq x y
      | (`Lifted2 x, `Lifted2 y) -> Base2.leq x y
      | _ -> false

  let pretty_diff () ((x:t),(y:t)): Pretty.doc = 
    if leq x y then Pretty.text "No Changes" else
    Pretty.dprintf "%a instead of %a" pretty x pretty y

  let oldjoin x y = 
    match (x,y) with 
      | (`Top, _) -> `Top
      | (_, `Top) -> `Top
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Lifted1 x, `Lifted1 y) -> begin
          try `Lifted1 (Base1.oldjoin x y) 
          with Unsupported _ -> `Top
        end
      | (`Lifted2 x, `Lifted2 y) -> begin
          try `Lifted2 (Base2.oldjoin x y) 
          with Unsupported _ -> `Top
        end
      | _ -> `Top

  let join x y = 
    match (x,y) with 
      | (`Top, `Top) -> `Equal
      | (`Top, _)    -> `Left
      | (_, `Top)    -> `Right
      | (`Bot, `Bot) -> `Equal
      | (`Bot, x)    -> `Right
      | (x, `Bot)    -> `Left
      | (`Lifted1 x, `Lifted1 y) -> begin
          try match Base1.join x y with
            | `Equal -> `Equal
            | `Left  -> `Left
            | `Right -> `Right
            | `New y -> `New (`Lifted1 y)
          with Unsupported _ -> `New `Top
        end
      | (`Lifted2 x, `Lifted2 y) -> begin
          try match Base2.join x y with
            | `Equal -> `Equal
            | `Left  -> `Left
            | `Right -> `Right
            | `New y -> `New (`Lifted2 y)
          with Unsupported _ -> `New `Top
        end
      | _ -> `New `Top

  let meet x y = 
    match (x,y) with
      | (`Bot, _) -> `Bot
      | (_, `Bot) -> `Bot
      | (`Top, x) -> x
      | (x, `Top) -> x
      | (`Lifted1 x, `Lifted1 y) -> begin
          try `Lifted1 (Base1.meet x y) 
          with Unsupported _ -> `Bot
        end
      | (`Lifted2 x, `Lifted2 y) -> begin
          try `Lifted2 (Base2.meet x y) 
          with Unsupported _ -> `Bot
        end
      | _ -> `Bot
      (*
  let join x y = 
    let d = oldjoin x y in
    match join x y with
      | `Equal -> assert (equal x y && equal d x); `Equal
      | `Left  -> assert (equal x d); `Left
      | `Right -> assert (equal y d); `Right
      | `New q -> assert (equal d q); `New q 
      *)
  let widen x y = 
    match (x,y) with 
      | (`Lifted1 x, `Lifted1 y) -> `Lifted1 (Base1.widen x y) 
      | (`Lifted2 x, `Lifted2 y) -> `Lifted2 (Base2.widen x y) 
      | _ -> y

  let narrow x y = 
    match (x,y) with 
      | (`Lifted1 x, `Lifted1 y) -> `Lifted1 (Base1.narrow x y) 
      | (`Lifted2 x, `Lifted2 y) -> `Lifted2 (Base2.narrow x y) 
      | _ -> x
      
end

module ProdConf (C: Printable.ProdConfiguration) (Base1: S) (Base2: S) =
struct
  include Printable.ProdConf (C) (Base1) (Base2)

  let bot () = (Base1.bot (), Base2.bot ())
  let is_bot (x1,x2) = Base1.is_bot x1 && Base2.is_bot x2
  let top () = (Base1.top (), Base2.top ())
  let is_top (x1,x2) = Base1.is_top x1 && Base2.is_top x2

  let leq (x1,x2) (y1,y2) = Base1.leq x1 y1 && Base2.leq x2 y2

  let pretty_diff () ((x1,x2:t),(y1,y2:t)): Pretty.doc = 
    if Base1.leq x1 y1 then
      Base2.pretty_diff () (x2,y2)
    else 
      Base1.pretty_diff () (x1,y1)

  let op_scheme op1 op2 (x1,x2) (y1,y2): t = (op1 x1 y1, op2 x2 y2)
  let oldjoin = op_scheme Base1.oldjoin Base2.oldjoin
  let join (x1,x2) (y1,y2) = 
    match Base1.join x1 y1, Base2.join x2 y2 with
      | (`Equal, `Equal) -> `Equal
      | (`Equal, `Left)  -> `Left
      | (`Equal, `Right) -> `Right
      | (`Left, `Equal)  -> `Left
      | (`Right, `Equal) -> `Right
      | (`Left, `Left)   -> `Left
      | (`Right, `Right) -> `Right
      | (x,y) -> `New (GU.descVal x1 y1 x,GU.descVal x2 y2 y)
(*  let join x y = 
    let d = oldjoin x y in
    match join x y with
      | `Equal -> assert (equal x y && equal d x); `Equal
      | `Left  -> (if not (equal x d) then ignore (Pretty.printf "joining\n%a\nwith\n%a\ninto\n%a\n" pretty x pretty y pretty d));assert (equal x d); `Left
      | `Right -> assert (equal y d); `Right
      | `New q -> assert (equal d q); `New q *)
  let meet = op_scheme Base1.meet Base2.meet
  let narrow = op_scheme Base1.narrow Base2.narrow
  let widen = op_scheme Base1.widen Base2.widen
end


module Prod = ProdConf (struct let expand_fst = true let expand_snd = true end)
module ProdSimple = ProdConf (struct let expand_fst = false let expand_snd = false end)

module LexProd (Base1: S) (Base2: S) =
struct
  include Prod (Base1) (Base2)

  let leq (x1,x2) (y1,y2) = 
    if Base1.equal x1 y1 then
      Base2.leq x2 y2
    else 
      Base1.leq x1 y1 
      
  let oldjoin (x1, y1) (x2, y2) =
    if Base1.equal x1 x2 then
      (x1, Base2.oldjoin y1 y2)
    else if Base1.leq x1 x2 then
      (x2, y2)
    else if Base1.leq x2 x1 then 
      (x1, y1)
    else
      (Base1.oldjoin x1 x2, Base2.bot ())    

  let join x y = `New (oldjoin x y)

  let meet (x1, y1) (x2, y2) =
    if Base1.equal x1 x2 then
      (x1, Base2.meet y1 y2)
    else if Base1.leq x1 x2 then
      (x2, y2)
    else if Base1.leq x2 x1 then 
      (x1, y1)
    else
      (Base1.meet x1 x2, Base2.top ())    
end

module Prod3 (Base1: S) (Base2: S) (Base3: S) =
struct
  include Printable.Prod3 (Base1) (Base2) (Base3)

  let bot () = (Base1.bot (), Base2.bot (), Base3.bot ())
  let is_bot (x1,x2,x3) = Base1.is_bot x1 && Base2.is_bot x2 && Base3.is_bot x3
  let top () = (Base1.top (), Base2.top (), Base3.top ())
  let is_top (x1,x2,x3) = Base1.is_top x1 && Base2.is_top x2 && Base3.is_top x3

  let leq (x1,x2,x3) (y1,y2,y3) = Base1.leq x1 y1 && Base2.leq x2 y2 && Base3.leq x3 y3

  let pretty_diff () ((x1,x2,x3:t),(y1,y2,y3:t)): Pretty.doc = 
    if not (Base1.leq x1 y1) then
      Base1.pretty_diff () (x1,y1)
    else if not (Base2.leq x2 y2) then
      Base2.pretty_diff () (x2,y2)
    else 
      Base3.pretty_diff () (x3,y3)

  let op_scheme op1 op2 op3 (x1,x2,x3) (y1,y2,y3): t = (op1 x1 y1, op2 x2 y2, op3 x3 y3)
  let oldjoin = op_scheme Base1.oldjoin Base2.oldjoin Base3.oldjoin
  let join (x1,x2,x3) (y1,y2,y3) = 
    match Base1.join x1 y1, Base2.join x2 y2, Base3.join x3 y3 with
      | (`Equal, `Left , `Left) 
      | (`Left , `Equal, `Left) 
      | (`Left , `Left , `Equal) 
      | (`Equal, `Equal, `Left) 
      | (`Equal, `Left , `Equal) 
      | (`Left , `Equal, `Equal) 
      | (`Left , `Left , `Left) -> `Left
      | (`Equal, `Right, `Right) 
      | (`Right, `Equal, `Right) 
      | (`Right, `Right, `Equal) 
      | (`Equal, `Equal, `Right) 
      | (`Equal, `Right, `Equal) 
      | (`Right, `Equal, `Equal) 
      | (`Right, `Right, `Right) -> `Right
      | (x,y,z) -> `New (GU.descVal x1 y1 x,GU.descVal x2 y2 y, GU.descVal x3 y3 z)  
(*  let join x y = 
    let d = oldjoin x y in
    match join x y with
      | `Equal -> assert (equal x y && equal d x); `Equal
      | `Left  -> assert (equal x d); `Left
      | `Right -> assert (equal y d); `Right
      | `New q -> assert (equal d q); `New q 
*)      
  let meet = op_scheme Base1.meet Base2.meet Base3.meet
  let widen = op_scheme Base1.widen Base2.widen Base3.widen
  let narrow = op_scheme Base1.narrow Base2.narrow Base3.narrow
end

module LiftBot (Base : S) =
struct
  include Printable.LiftBot (Base)

  let bot () = `Bot
  let is_bot x = x = `Bot
  let top () = `Lifted (Base.top ())
  let is_top x = 
    match x with 
      | `Lifted x -> Base.is_top x
      | `Bot -> false

  let leq x y =
    match (x,y) with
      | (`Bot, _) -> true
      | (_, `Bot) -> false
      | (`Lifted x, `Lifted y) -> Base.leq x y

  let pretty_diff () ((x:t),(y:t)): Pretty.doc = 
    if leq x y then Pretty.text "No Changes" else
    Pretty.dprintf "%a instead of %a" pretty x pretty y

  let oldjoin x y = 
    match (x,y) with 
      | (`Bot, x) -> x
      | (x, `Bot) -> x
      | (`Lifted x, `Lifted y) -> `Lifted (Base.oldjoin x y)
      
  let join x y = 
    match (x,y) with 
      | (`Bot, `Bot) -> `Equal
      | (`Bot, x) -> `Right
      | (x, `Bot) -> `Left
      | (`Lifted x, `Lifted y) -> 
    match Base.join x y with 
      | `Equal -> `Equal
      | `Left  -> `Left
      | `Right -> `Right
      | `New y -> `New (`Lifted y)

(*  let join x y = 
    let d = oldjoin x y in
    match join x y with
      | `Equal -> assert (equal x y && equal d x); `Equal
      | `Left  -> assert (equal x d); `Left
      | `Right -> assert (equal y d); `Right
      | `New q -> assert (equal d q); `New q 
      *)
  let meet x y = 
    match (x,y) with 
      | (`Bot, _) -> `Bot
      | (_, `Bot) -> `Bot
      | (`Lifted x, `Lifted y) -> `Lifted (Base.meet x y) 
  
  let widen x y =
    match (x,y) with
      | (`Lifted x, `Lifted y) -> `Lifted (Base.widen x y)
      | _ -> y

  let narrow x y =
    match (x,y) with
      | (`Lifted x, `Lifted y) -> `Lifted (Base.narrow x y)
      | _ -> x
end
  
module LiftTop (Base : S) =
struct
  include Printable.LiftTop (Base)

  let top () = `Top
  let is_top x = x = `Top
  let bot () = `Lifted (Base.bot ())
  let is_bot x = 
    match x with 
      | `Lifted x -> Base.is_bot x
      | `Top -> false

  let leq x y =
    match (x,y) with
      | (_, `Top) -> true
      | (`Top, _) -> false
      | (`Lifted x, `Lifted y) -> Base.leq x y

  let oldjoin x y = 
    match (x,y) with 
      | (`Top, x) -> `Top
      | (x, `Top) -> `Top
      | (`Lifted x, `Lifted y) -> `Lifted (Base.oldjoin x y)

  let join x y = 
    match (x,y) with 
      | (`Top, `Top) -> `Equal
      | (`Top, x)    -> `Left
      | (x, `Top)    -> `Right
      | (`Lifted x, `Lifted y) -> 
    match Base.join x y with
      | `Equal -> `Equal
      | `Left  -> `Left
      | `Right -> `Right
      | `New y -> `New (`Lifted y)
      
(*  let join x y = 
    let d = oldjoin x y in
    match join x y with
      | `Equal -> assert (equal x y && equal d x); `Equal
      | `Left  ->  (if not (equal x d) then ignore (Pretty.printf "joining\n%a\nwith\n%a\ninto\n%a\n" pretty x pretty y pretty d)); assert (equal x d); `Left
      | `Right -> assert (equal y d); `Right
      | `New q -> assert (equal d q); `New q*) 

  let meet x y = 
    match (x,y) with 
      | (`Top, x) -> x
      | (x, `Top) -> x
      | (`Lifted x, `Lifted y) -> `Lifted (Base.meet x y) 

  let widen x y =
    match (x,y) with
      | (`Lifted x, `Lifted y) -> `Lifted (Base.widen x y)
      | _ -> y

  let narrow x y =
    match (x,y) with
      | (`Lifted x, `Lifted y) -> `Lifted (Base.narrow x y)
      | _ -> x
end
  
module Either (B1: S) (B2: S) =
struct 
  include Printable.Either (B1) (B2)
  let top () = `Left (B1.top ())
  let bot () = `Right (B2.bot ())
  let is_top = function 
    | `Left x -> B1.is_top x
    | `Right x -> false
  let is_bot = function 
    | `Left x -> false
    | `Right x -> B2.is_bot x
  let leq x y =
    match x, y with
      | `Left  x, `Left  y -> B1.leq x y
      | `Right x, `Right y -> B2.leq x y
      | `Left  _, `Right _ -> false
      | `Right _, `Left  _ -> true
  let join x y =
    match x, y with
      | `Left  x, `Left  y -> GU.liftDesc (fun x -> `Left x) (B1.join x y)
      | `Right x, `Right y -> GU.liftDesc (fun x -> `Right x) (B2.join x y)
      | `Left  _, `Right _ -> `Left
      | `Right _, `Left  _ -> `Right
  let oldjoin x y =
    match x, y with
      | `Left  x, `Left  y -> `Left (B1.oldjoin x y)
      | `Right x, `Right y -> `Right (B2.oldjoin x y)
      | `Left  _, `Right _ -> x
      | `Right _, `Left  _ -> y
  let meet x y =
    match x, y with
      | `Left  x, `Left  y -> `Left (B1.meet x y)
      | `Right x, `Right y -> `Right (B2.meet x y)
      | `Left  _, `Right _ -> y
      | `Right _, `Left  _ -> x
  let widen x y =
    match x, y with
      | `Left  x, `Left  y -> `Left  (B1.widen x y)
      | `Right x, `Right y -> `Right (B2.widen x y)
      | _ -> failwith "invalid argument for widen"
  let narrow x y =
    match x, y with
      | `Left  x, `Left  y -> `Left  (B1.narrow x y)
      | `Right x, `Right y -> `Right (B2.narrow x y)
      | _ -> failwith "invalid argument for narrow"
end                             
  
module Liszt (Base: S) = 
struct
  include Printable.Liszt (Base)
  include StdCousot
  let bot () = raise (Unsupported "bot?") 
  let is_top _ = false
  let top () = raise (Unsupported "top?")
  let is_bot _ = false

  let rec leq = 
    let f acc x y = Base.leq x y && acc in
      List.fold_left2 f true

  let join x y = 
    let f (p,po) x y =
      match Base.join x y with
        | `Equal -> (x::p, GU.joinDesc po `Equal   )
        | `Left  -> (x::p, GU.joinDesc po `Left   )
        | `Right -> (y::p, GU.joinDesc po `Right  )
        | `New q -> (q::p, Some `New)
    in
    match List.fold_left2 f ([], None) x y with
      | _, Some `Equal -> `Equal
      | _, Some `Left  -> `Left
      | _, Some `Right -> `Right
      | [], None -> `Equal
      | r , _    -> `New (List.rev r)   
  let oldjoin = List.map2 Base.oldjoin
(*  let join x y = 
    let d = oldjoin x y in
    match join x y with
      | `Equal -> assert (equal x y && equal d x); `Equal
      | `Left  -> assert (equal x d); `Left
      | `Right -> assert (equal y d); `Right
      | `New q -> assert (equal d q); `New q 
*)  
  let meet = List.map2 Base.meet
end

module Chain (P: Printable.ChainParams) = 
struct
  include Printable.Std
  include Printable.Chain (P)
  include StdCousot
  let bot () = 0
  let is_bot x = x = 0
  let top () = P.n - 1
  let is_top x = x = P.n - 1

  let leq x y = x <= y
  let join x y = 
      if x=y then `Equal else if x>y then `Left else `Right
  let oldjoin x y = max x y
  let meet x y = min x y
end
