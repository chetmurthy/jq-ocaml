
open Jqparse0

let parse_functions_from_file fname =
  parse_file parse_funcdefs_eoi fname

let exec ?(top=false) e =
  if top then
    let functions = Jqparse0.(parse_file parse_funcdefs_eoi "builtin.jq") in
    I.exec ~functions e
  else
    I.exec e

