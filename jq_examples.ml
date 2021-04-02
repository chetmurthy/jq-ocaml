open Asttools ;;
open Lazy_reclist ;;
open Jqutil ;;

let object_field = object_field ~path:false

let eg = Yojson.Basic.from_file "eg.json" ;;

(* . *)

[eg] |> of_list |> map (function v -> Left v) |> to_list
;;

(* .[0] *)

[eg] |> of_list |> map (function j -> Left (array_deref 0 j)) |> to_list
;;

(* '.[0] | {message: .commit.message, name: .commit.committer.name}' *)

[eg] |> of_list
|> map ((function j -> Left (array_deref 0 j)))
|> map (function j -> Left (`Assoc[("message", j |> object_field "commit" |> object_field "message")
                                  ;("name", j |> object_field "commit" |> object_field "committer" |> object_field "name")]))
|> to_list
;;

(* '.[] | {message: .commit.message, name: .commit.committer.name}' *)

[eg] |> of_list
|> map ((function j -> Right (array_list j)))
|> map (function j -> Left (`Assoc[("message", j |> object_field "commit" |> object_field "message")
                                  ;("name", j |> object_field "commit" |> object_field "committer" |> object_field "name")]))
|> to_list
;;

(* '[.[] | {message: .commit.message, name: .commit.committer.name}]' *)

[eg] |> of_list
|> map (fun j -> Left (j |> gather_to_array
                         (fun j ->
                            j |> array_list
                            |> map (function j ->
                                Left (`Assoc[("message", j |> object_field "commit" |> object_field "message")
                                            ;("name", j |> object_field "commit" |> object_field "committer" |> object_field "name")
                                            ])))))
|> to_list
;;

(* '[.[] | {message: .commit.message, name: .commit.committer.name, parents: [.parents[].html_url]}]' *)

[eg] |> of_list
|> map (fun j -> Left (j |> gather_to_array
                         (fun j ->
                            j |> array_list
                            |> map (function j ->
                                Left (`Assoc[("message", j |> object_field "commit" |> object_field "message")
                                            ;("name", j |> object_field "commit" |> object_field "committer" |> object_field "name")
                                            ;("parent",
                                              j |> gather_to_array (fun j ->
                                                  j |> object_field "parents" |> singleton
                                                  |> map (function j -> Right(array_list j))
                                                  |> map (function j -> Left(object_field "html_url" j))))
                                            ])))))
|> to_list
;;
