open Asttools ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Jqtypes ;

type t += [
    Exc of Ploc.t and t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]
] [@@deriving show;]
;

value print_exn exn = Some (show exn) ;
Printexc.register_printer print_exn ;

value input_file = ref "" ;

value lexer = do {
  Plexer.dollar_for_antiquotation.val := False ;
  let rv = Plexer.gmake() in
  Plexer.dollar_for_antiquotation.val := True ;
  rv
} ;
value g = Grammar.gcreate lexer;
value (exp : Grammar.Entry.e exp) = Grammar.Entry.create g "exp";
value (exp_eoi : Grammar.Entry.e exp) = Grammar.Entry.create g "exp_eoi";

EXTEND
  GLOBAL:
    exp exp_eoi
    ;

  exp: [
      "simple" [
        "." -> ExpDot
      | ".." -> ExpDotDot
      | "$" ; id = LIDENT -> ExpDataVar id
    ] ]
  ;
  exp_eoi : [ [ e = exp ; EOI -> e ] ] ;
END;

value parse_exp = Grammar.Entry.parse exp ;
value parse_exp_eoi = Grammar.Entry.parse exp_eoi ;

value parse_string pf s =
  pf (Stream.of_string s)
;

value parse_channel pf ic =
  pf (Stream.of_channel ic)
;

value parse_file pf fname =
  let ic = open_in fname in
  let rv = pf (Stream.of_channel ic) in 
  do { close_in ic ; rv }
;
