
module type OrderedType = Set.OrderedType

module type S =
sig

  type t
  (** Type similar to standard sets, below are the standard operations on it *)

  type keys
  (** keys is an internal type that should not be used directly by the user *)

  type +'a k_arrows
  (** defines the arrows necessary for nested sets *)


  val empty: t
  val is_empty: t -> bool

  val mem: ( t -> bool ) k_arrows
  val add: ( t -> t ) k_arrows
  val singleton: t k_arrows
  val remove: ( t -> t ) k_arrows
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (unit k_arrows) -> t -> unit
  val fold: (( 'a -> 'a ) k_arrows) -> t -> 'a -> 'a
  val for_all: (bool k_arrows) -> t -> bool
  val exists: (bool k_arrows) -> t -> bool
  val filter: (bool k_arrows) -> t -> t
  val partition: (bool k_arrows) -> t -> t * t
  val cardinal: t -> int
  val split: ( t -> t * bool * t ) k_arrows

  val fold_args : (keys -> 'a) -> 'a k_arrows
  (** fold_args and unfold_args allow to manipulate the internal representation key
      and to iterate through the sets without knowing the actual number of arguments.
      fold_args turns a non-currified function into a currified one.
  *)

  val unfold_args : 'a k_arrows -> keys -> 'a
  (** as fold_args, allow to turn a currified function into a non-currified one *)

end

(** Similar to Set.Make but compatible with signature S above *)
module Make : functor ( Ord : OrderedType ) ->
  S with type t = Set.Make(Ord).t
     and type 'a k_arrows = Ord.t -> 'a

(** Adds a key on top of the set, everything stays currified *)
module Nest : functor (Ord : OrderedType) -> functor (S : S) ->
  S with type t = S.t Map.Make(Ord).t
     and type 'a k_arrows = Ord.t -> 'a S.k_arrows

(** Base module for printable types *)
module type PrintableType =
sig
  type t
  val print : Format.formatter -> t -> unit
end

(** Printable key type *)
module type OrderedPrintableType =
sig
  type t
  include OrderedType with type t := t
  include PrintableType with type t := t
end

(** Printable map type *)
module type SPrintable =
sig
  type t
  include S with type t := t
  include PrintableType with type t := t
end

(** Make a printable Set *)
module MakePrint :
  functor (Ord : OrderedPrintableType) ->
    SPrintable
  with type t = Make(Ord).t
   and type 'a k_arrows = 'a Make(Ord).k_arrows
   and type keys = Make(Ord).keys

(** Nest a printable Set *)
module NestPrint :
  functor (Ord : OrderedPrintableType) ->
  functor (S : SPrintable) ->
    SPrintable
  with type t = Nest(Ord)(S).t
   and type 'a k_arrows = 'a Nest(Ord)(S).k_arrows
