(* This file implements simple graph with labels on the edges.
   This aims at showing how playing with different interfaces over the same type
   can provide really enjoyable coding.

   Note that the type definition are quite longer and can be complex, also the
   type error messages can be hard to read. We advise you give very explicit
   names to your modules and keep manually track of the type equalities.
 *)

module Node =
struct
  type t = int
  let compare (a:t) b = compare a b
end

module Edge =
struct
  type t = string
  let compare (a:t) b = compare a b
end

module Int =
struct
  type t = int
  let compare (a:t) b = compare a b
end

(* First the standard maps *)
module Nm = Map.Make (Node)
module Em = Map.Make (Edge)
(* and a standard set *)
module Ns = Set.Make (Node)

(* Simple set, similar to Ns *)
module Nodes = CurrySet.Make (Node)

(* Emap is similar to Em *)
module Emap =  CurryMap.Make (Edge)

(* Graph provides a type similar to Nodes.t Em.t Nm.t *)
module Graph = CurryMap.Bind ( CurryMap.Nest (Node) (Emap) ) (Nodes)

(* EdgeNodeSet is similar to Nodes.t Em.t and can represent the set of children
   of a node *)
module EdgeNodeSet = CurrySet.Nest (Edge) (Nodes)

(* GraphSet is similar to Graph but is seen as a set of edges *)
module GraphSet = CurrySet.Nest (Node) (EdgeNodeSet)

(* Representation of the cost of each edge *)
module EdgeCosts =
  CurryMap.SafeFind (
    CurryMap.Bind
      (CurryMap.Nest (Node)
         (CurryMap.Nest (Edge) (CurryMap.Make (Node) ) )
      )
      (Int)
  ) (struct type t = int let bottom = max_int / 2 end)

let label0 = "label_0"
let label1 = "label_1"

let simple_graph : Graph.t =
  let open GraphSet in
  (* We can easily construct that graph by viewing it as a set of edges...
     and then see it as a Graph.t!
  *)
  empty
  |> add 0 label0 1
  |> add 0 label0 2
  |> add 1 label1 0
  |> add 1 label0 2
  |> add 2 label0 1

let simple_costs =
  let open EdgeCosts in
  empty
  |> add 0 label0 1 4
  |> add 0 label0 2 5
  |> add 1 label1 0 2
  |> add 1 label0 2 155
(* |> add 2 label0 1 1 *) (* we don't put anything here so the cost will be max_int / 2 *)

let dijkstra n goal graph costs =

  let module Im = Map.Make(Int) in
  let module CostN = CurrySet.Nest (Int) (CurrySet.Make(Node)) in
  let module NCost =
    CurryMap.SafeFind
      (CurryMap.Bind (CurryMap.Make (Node)) (Int) )
      (struct type t = int let bottom = max_int end)
  in

  let costs_init = CostN.singleton 0 n in
  let ncosts_init = NCost.singleton n 0 in
  let visited_init = Nodes.singleton n in
  let best_paths_init = Nm.singleton n [] in

  let rec findpath visited dist_to_n n_to_dist best_paths n curr_cost =
    if Node.compare n goal = 0
    then List.rev @@ Nm.find n best_paths
    else
      let visited = Nodes.add n visited in
      let dist_to_n, n_to_dist , best_paths =
        EdgeNodeSet.fold (fun label n' (dist_to_n, n_to_dist, best_paths) ->
            if Nodes.mem n' visited
            then dist_to_n, n_to_dist, best_paths
            else
              let ecost = EdgeCosts.find n label n' costs in
              let new_cost = curr_cost + ecost in
              let old_cost = NCost.find n' n_to_dist in
              if new_cost < max_int / 2 && old_cost > new_cost
              then
                ( CostN.add new_cost n' dist_to_n,
                  NCost.add n' new_cost n_to_dist,
                  Nm.add n' (label::(Nm.find n best_paths)) best_paths)
              else dist_to_n, n_to_dist, best_paths
          ) (Nm.find n graph) (dist_to_n, n_to_dist, best_paths)
      in
      let _,_,dist_to_n = CostN.split curr_cost n dist_to_n in
      let cost, next_to_visit =
        try Im.min_binding dist_to_n
        with Not_found ->
          raise Exit
      in
      let next_to_visit = Nodes.diff next_to_visit visited in
      findpath visited dist_to_n n_to_dist best_paths (Ns.min_elt next_to_visit) cost

  in

  findpath visited_init costs_init ncosts_init best_paths_init n 0


let () =
  try
    print_endline @@
    String.concat ", " (dijkstra 1 2 simple_graph simple_costs)
  with Exit -> print_endline "No path found"
        
      
