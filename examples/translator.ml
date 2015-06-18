(* This file provides a simple nested map over different datas *)


module String = String

module M = CurryMap.SafeFind
    (CurryMap.Bind
       (CurryMap.Nest ( String ) ( CurryMap.Make ( String ) ) ) (String) )
    (struct type t = string let bottom = "" end)

open M

let en = "en"
let fr = "fr"

let translate_db : t =
  empty
  |> add en "hello" "hello"
  |> add fr "hello" "bonjour"
  |> add en "good bye" "good bye"
  |> add fr "good bye" "au revoir"
  |> add en "thanks" "thanks"
  |> add fr "thanks" "merci"

let translate lang word = find lang word translate_db

let () = print_endline @@ translate "fr" "hello"
