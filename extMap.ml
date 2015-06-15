


module type Type = sig type t end
module type OrderedType = Map.OrderedType
module type BottomedType =
sig
  type t
  val bottom : t
end

module type S =
sig

  type 'a t
  type keys
  type +'a k_arrows

  val empty: 'a t
  val is_empty: 'a t -> bool
  val mem:  ( 'a t -> bool ) k_arrows
  val add: ( 'a -> 'a t -> 'a t ) k_arrows
  val singleton: ( 'a -> 'a t ) k_arrows
  val remove: ( 'a t -> 'a t ) k_arrows
  val merge:
    (( 'a option -> 'b option -> 'c option ) k_arrows) -> 'a t ->  'b t -> 'c t
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter: (( 'a -> unit ) k_arrows) -> 'a t -> unit
  val fold: (( 'a -> 'b -> 'b ) k_arrows) -> 'a t -> 'b -> 'b
  val for_all: (( 'a -> bool ) k_arrows) -> 'a t -> bool
  val exists: (( 'a -> bool ) k_arrows) -> 'a t -> bool
  val filter: (( 'a -> bool ) k_arrows) -> 'a t -> 'a t
  val partition: (( 'a -> bool ) k_arrows) -> 'a t -> 'a t * 'a t
  val cardinal: 'a t -> int
  val split: ( 'a t -> 'a t * 'a option * 'a t ) k_arrows
  val find: ( 'a t -> 'a ) k_arrows
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (( 'a -> 'b ) k_arrows) -> 'a t -> 'b t

  val fold_args : (keys -> 'a) -> 'a k_arrows
  val unfold_args : 'a k_arrows -> keys -> 'a
end


module type SBound =
sig

  type t
  type elt
  type keys
  type +'a k_arrows

  val empty: t
  val is_empty: t -> bool
  val mem:  ( t -> bool ) k_arrows
  val add: ( elt -> t -> t ) k_arrows
  val singleton: ( elt -> t ) k_arrows
  val remove: ( t -> t ) k_arrows
  val merge:
    (( elt option -> elt option -> elt option ) k_arrows) -> t ->  t -> t
  val compare: (elt -> elt -> int) -> t -> t -> int
  val equal: (elt -> elt -> bool) -> t -> t -> bool
  val iter: (( elt -> unit ) k_arrows) -> t -> unit
  val fold: (( elt -> 'a -> 'a ) k_arrows) -> t -> 'a -> 'a
  val for_all: (( elt -> bool ) k_arrows) -> t -> bool
  val exists: (( elt -> bool ) k_arrows) -> t -> bool
  val filter: (( elt -> bool ) k_arrows) -> t -> t
  val partition: (( elt -> bool ) k_arrows) -> t -> t * t
  val cardinal: t -> int
  val split: ( t -> t * elt option * t ) k_arrows
  (* val find_unsafe: ( t -> elt ) k_arrows *)
  val find: ( t -> elt ) k_arrows
  val map: (elt -> elt) -> t -> t
  val mapi: (( elt -> elt ) k_arrows) -> t -> t

  val fold_args : (keys -> 'a) -> 'a k_arrows
  val unfold_args : 'a k_arrows -> keys -> 'a
end

module Make ( Ord : OrderedType )
  : S with type 'a t = 'a Map.Make(Ord).t
       and type 'a k_arrows = Ord.t -> 'a
=
struct

  module M = Map.Make(Ord)

  include (M : Map.S with type 'a t := 'a M.t and type key = Ord.t)

  type 'a t = 'a M.t
  type 'a k_arrows = Ord.t -> 'a
  type keys = Ord.t

  (* let find_unsafe = find *)
  (* let find x m = try find x m with Not_found -> default *)

  let fold_args f = f
  let unfold_args f = f


end

module Nest (Ord : OrderedType) (M : S) :
  S with type 'a t = 'a M.t Map.Make(Ord).t
     and type 'a k_arrows = Ord.t -> 'a M.k_arrows
=
struct
  module MM = Map.Make(Ord)

  (* include (MM : Map.S with type 'a t := 'a MM.t and type key = Ord.t) *)

  type 'a t = 'a M.t MM.t
  type 'a k_arrows = Ord.t -> 'a M.k_arrows
  type keys = Ord.t * M.keys

  let fold_args (f: keys -> 'a) : 'a k_arrows =
    fun k -> M.fold_args (fun rest -> f (k,rest))
  let unfold_args f (k,r) = M.unfold_args (f k) r

  let empty = MM.empty      
  let is_empty m = MM.for_all (fun _ m' -> M.is_empty m') m

  let mem_inside (k,rest) m =
    try MM.find k m |> M.unfold_args M.mem rest
    with Not_found -> false

  let mem k = M.fold_args (fun rest -> mem_inside (k,rest))

  let add_inside (k,rest) v m =
    try let m' = MM.find k m in
      MM.add k ( M.unfold_args M.add rest v m' ) m
    with Not_found -> MM.add k ( M.unfold_args M.singleton rest v ) m

  let add k = M.fold_args (fun rest -> add_inside (k,rest))

  let singleton_inside (k,rest) v = MM.singleton k @@ M.unfold_args M.singleton rest v

  let singleton k = M.fold_args (fun rest -> singleton_inside (k,rest))

  let remove_inside =
    (fun ((k,rest):keys) (m: 'a M.t MM.t) ->
       try let m' = MM.find k m in MM.add k (M.unfold_args M.remove rest m') m
       with Not_found -> m)


  let remove k = M.fold_args (fun rest -> remove_inside (k,rest))


  let merge f m m' =
    MM.merge
      (fun k o o' ->
         let unoption = function
           | Some m -> m
           | None -> M.empty
         in
         let m = unoption o
         and m' = unoption o' in
         let res = M.merge (f k) m m' in
         if M.is_empty res
         then None
         else Some res
      ) m m'

  let compare c m m' =
    MM.compare (M.compare c) m m'

  let equal e m m' =
    MM.equal (M.equal e) m m'

  let descend_simple mmf mf f m =
    mmf (fun k m -> mf (f k) m) m

  let iter f m = descend_simple MM.iter M.iter f m

  let fold f m acc =
    MM.fold (fun k m acc -> M.fold (f k) m acc) m acc

  let for_all f m = descend_simple MM.for_all M.for_all f m
  let exists f m = descend_simple MM.exists M.exists f m

  let filter f m =
    MM.fold (fun k m acc -> MM.add k (M.filter (f k) m) acc) m MM.empty

  let partition f m =
    MM.fold (fun k m (r,r') ->
        let sr,sr' = M.partition (f k) m in
        MM.add k sr r, MM.add k sr' r') m (MM.empty,MM.empty)

  let split_inside =
    (fun (k,rest) m ->
       let minf,o,msup = MM.split k m in
       match o with
       | Some m ->
         let minf',o,msup' = M.unfold_args M.split rest m in
         MM.add k minf' minf, o, MM.add k msup' msup
       | None ->  minf,None,msup)

  let split k = M.fold_args (fun rest -> split_inside (k,rest))

  let find_inside =
    (fun (k,rest) m ->
       let m = MM.find k m in
       M.unfold_args M.find rest m)

  let find k = M.fold_args (fun rest -> find_inside (k,rest))

  (* let find = fold_args *)
  (*     (fun (k,rest) m -> *)
  (*        try *)
  (*          let m = MM.find k m in *)
  (*          M.unfold_args M.find_unsafe rest m *)
  (*        with Not_found -> default) *)

  let map f m =
    MM.map (fun m -> M.map f m) m

  let mapi f m =
    MM.mapi (fun k m -> M.mapi (f k) m) m

  let cardinal m = MM.fold (fun _ m acc -> (M.cardinal m) + acc) m 0

end

module Bind (M : S) (T : Type) : SBound
  with type t = T.t M.t
   and type 'a k_arrows = 'a M.k_arrows
   and type elt = T.t
   and type keys = M.keys
=
struct
  include (M : S
           with type 'a t := 'a M.t
            and type 'a k_arrows = 'a M.k_arrows
            and type keys = M.keys)

  type elt = T.t
  type t = T.t M.t

end

module SafeFind (M : SBound) (O : BottomedType with type t = M.elt): SBound
  with type t = M.t
   and type 'a k_arrows = 'a M.k_arrows
   and type elt = M.elt
   and type keys = M.keys
=
struct
  include M

  let find = fold_args
      (fun ks m -> try unfold_args find ks m with Not_found -> O.bottom)
end

(* printing options *)
module type PrintableType =
sig
  type t
  val print : Format.formatter -> t -> unit
end

module type PrintablePolyType =
sig
  type 'a t
  val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type OrderedPrintableType =
sig
  type t
  include OrderedType with type t := t
  include PrintableType with type t := t
end

module type SPrintable =
sig
  type 'a t
  include S with type 'a t := 'a t
  include PrintablePolyType with type 'a t := 'a t
end

module type SBoundPrintable =
sig
  type t
  include SBound with type t := t
  include PrintableType with type t := t
end

module MakePrint (Ord : OrderedPrintableType) :
  SPrintable
  with type 'a t = 'a Make(Ord).t
   and type 'a k_arrows = 'a Make(Ord).k_arrows
   and type keys = Make(Ord).keys
=
struct

  include Make(Ord)

  let print f ppf m =
    Format.pp_open_hvbox ppf 2;
    iter
      (fun k v ->
         Format.fprintf ppf "@[<hov 2>%a@ ->@ %a@]@,"
           Ord.print k
           f v) m;
    Format.pp_close_box ppf ()

end

module NestPrint (Ord : OrderedPrintableType) (M : SPrintable) :
  SPrintable
  with type 'a t = 'a Nest(Ord)(M).t
   and type 'a k_arrows = 'a Nest(Ord)(M).k_arrows
   and type keys = Nest(Ord)(M).keys
=
struct

  include Nest (Ord) (M)

  module MM = Map.Make(Ord)

  let print f ppf m =
    Format.pp_open_hvbox ppf 2;
    MM.iter
      (fun k v ->
         Format.fprintf ppf
           "@[<hov 2>%a@ ->@ @[<hov 2>{@ %a@ }@]@]"
           Ord.print k (M.print f) v
      ) m;
    Format.pp_close_box ppf ()

end

module BindPrint (M : SPrintable) (B : PrintableType) :
  SBoundPrintable
  with type t = B.t M.t
   and type 'a k_arrows = 'a M.k_arrows
   and type keys = M.keys
   and type elt = B.t
=
struct
  include Bind (M) (B)

  let print ppf m = M.print B.print ppf m
end
