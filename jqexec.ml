
open Jqparse0
open Jqinterp

let parse_functions_from_file fname =
  parse_file parse_funcdefs_eoi fname

let _builtins = ref []

let _builtins () =
  if [] =  !_builtins then
    _builtins := Jqparse0.(parse_file parse_funcdefs_eoi "builtin.jq") ;
  !_builtins

let exec ?(builtins=false) e =
  if builtins then
    let l = _builtins() in
    let (fenv, [], []) = I.interp0_funcdefs (!I.predefined_functions, [], []) l in
    I.exec ~fenv e
  else
    I.exec e

