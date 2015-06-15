# curry-containers
Curryfied nested maps and sets compatible with OCaml's standard library maps and sets

This little package extends OCaml's maps and sets so that you can easily create and manipulate nested maps and sets.

## Motivations

Basically, you may often want to declare types such as

```ocaml
module Int = struct type t = int let compare = compare end

module Imap = Map.Make ( Int )
module Smap = Map.Make ( String )

type nmap = int Imap.t Smap.t Imap.t

let nmap_find i s i' m =
  try Imap.find i m |> Smap.find s |> Imap.find i'
  with Not_found -> default_value

let nmap_fold f m acc =
  Imap.fold (fun i m acc ->
    Smap.fold (fun s m acc ->
      Imap.fold
        (fun i' i'' acc -> f i s i' i'' acc)
        m acc)
      m acc)
    m acc
```

Then, you have to redefine folds and other accessors. That can be quite painful.

One solution would be to declare something as `module ISImap = Map.Make(struct type t = int * string * int let compare = compare end)`
but you'd then have to manipulate tuples and loose memory space (providing your first keys are often re-used).

## Our solution

This tiny library aims at providing nested maps still compatible with the standard library, but with nice currification:

```ocaml
module Int = struct type t = int let compare = compare end

module ISImap = ExtMap.Nest (Int) (ExtMap.Nest (String) (ExtMap.Make (Int)))
module ISImapi = ExtMap.SafeFind ( ExtMap.Bind (ISImap) (Int) ) (struct type t = int let bottom = 0 end)

type nmap = ISImapi.t

let nmam_find i s i' m = ISImapi.find i s i' m
```

Those nested maps are both simple to manipulate and still compatible with the standard library maps.

The same goes with nested sets, that is definitions like `Iset.t Imap.t Smap.t Smap.t` that you would like
to see as a set over `string * string * int * int`.

## Limitations

As of right now, functions supposed to return keys are not allowed
* For maps: `bindings`, `min_binding`, `max_binding` and `choose`
* For sets: `elements`, `min_element`, `max_element`, `choose` and `find`

Also, the module declarations may be quite heavy and it is recommended to duly comment them.
