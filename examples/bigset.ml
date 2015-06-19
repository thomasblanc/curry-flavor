module Int =
struct
  type t = int
  let compare (a:t) b = compare a b
end

module M = CurrySet.Nest (Int) (CurrySet.Nest (Int) (CurrySet.Make (Int) ) )

open M

let myset =
  empty
  |> add 0 0 0
  |> add 0 1 1
  |> add 1 0 1
  |> add 1 1 1

let () = iter (Printf.printf "%d %d %d") myset

