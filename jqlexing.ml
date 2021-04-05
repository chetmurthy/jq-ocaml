open Jqtypes
open Jqutil

let gen_of_string s =
  let pos = ref 0 in
  fun () ->
    if !pos = String.length s then None
    else let c = String.get s !pos in
      pos := !pos + 1 ;
      Some c

let ws = [%sedlex.regexp? ' ' | '\t' | '\r' | '\n']

let octdigit = [%sedlex.regexp? '0'..'7']
let digit = [%sedlex.regexp? '0'..'9']
let hexdigit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F']
let int = [%sedlex.regexp? '0' | ( ('1'..'9') , (Star digit) )]
let frac = [%sedlex.regexp? '.' , (Star digit)]
let ne_frac = [%sedlex.regexp? '.' , (Plus digit)]
let exp = [%sedlex.regexp? ('e' | 'E') , (Opt ('-' | '+')) , (Plus digit)]
let decimal_float_number = [%sedlex.regexp? (Opt '-') , ((int , (Opt frac) , (Opt exp)) | (ne_frac, Opt exp))]
let json_number = [%sedlex.regexp? (Opt '-') , int, Opt ne_frac, Opt exp]

let letter = [%sedlex.regexp? 'a'..'z'|'A'..'Z']

let alphanum = [%sedlex.regexp? (letter|digit)]
let ident = [%sedlex.regexp? (letter| '_'), Star (alphanum | '_')]

let json_unescaped = [%sedlex.regexp? 0x20 .. 0x21 | 0x23 .. 0x5B | 0x5D .. 0x10FFFF ]
let json_escaped = [%sedlex.regexp? "\\" , ( 0x22 | 0x5C | 0x2F | 0x62 | 0x66 | 0x6E | 0x72 | 0x74 | (0x75, Rep(hexdigit,4)) ) ]
let json_string_char = [%sedlex.regexp? (json_unescaped | json_escaped ) ]
let json_string = [%sedlex.regexp?  '"' , (Star json_string_char) , '"']

let comment = [%sedlex.regexp? '#' , Star(Compl '\n') ]

let readn n lb =
  let buf = Buffer.create 23 in
  let rec rerec = function
      0 -> Buffer.contents buf
    | n -> begin match%sedlex lb with
          any ->
          Buffer.add_utf_8_uchar buf (Sedlexing.lexeme_char lb 0) ;
          rerec (n-1)
        | _ -> rerec 0
      end
  in rerec n

let rec rawtoken buf =
  let pos() = Sedlexing.lexing_positions buf in
  match%sedlex buf with
  | Opt '-' , int -> (Integer (Sedlexing.Latin1.lexeme buf),pos())
  | json_number -> (Float (Sedlexing.Latin1.lexeme buf),pos())
  | json_string -> (String (Sedlexing.Latin1.lexeme buf),pos())
  | "[" -> (Spcl "[", pos())
  | "]" -> (Spcl "]", pos())
  | "{" -> (Spcl "{",pos())
  | "}" -> (Spcl "}",pos())
  | "(" -> (Spcl "(",pos())
  | ")" -> (Spcl ")",pos())
  | ":" -> (Spcl ":",pos())
  | "|" -> (Spcl "|",pos())
  | "+" -> (Spcl "+",pos())
  | "*" -> (Spcl "*",pos())
  | "/" -> (Spcl "/",pos())
  | "//" -> (Spcl "//",pos())
  | "%" -> (Spcl "%",pos())
  | ">" -> (Spcl ">",pos())
  | "<" -> (Spcl "<",pos())
  | ">=" -> (Spcl ">=",pos())
  | "<=" -> (Spcl "<=",pos())
  | "=" -> (Spcl "=",pos())
  | "==" -> (Spcl "==",pos())
  | "!=" -> (Spcl "!=",pos())
  | ":" -> (Spcl ":",pos())
  | ";" -> (Spcl ";",pos())
  | "," -> (Spcl ",",pos())
  | "." -> (Spcl ".",pos())
  | ".." -> (Spcl "..",pos())
  | "-" -> (Spcl"-",pos())
  | "$" -> (Spcl"$",pos())
  | "?" -> (Spcl"?",pos())
  | "def" -> (Keyw"def",pos())
  | "as" -> (Keyw"as",pos())
  | "reduce" -> (Keyw"reduce",pos())
  | "foreach" -> (Keyw"foreach",pos())
  | "if" -> (Keyw"if",pos())
  | "then" -> (Keyw"then",pos())
  | "elif" -> (Keyw"elif",pos())
  | "else" -> (Keyw"else",pos())
  | "end" -> (Keyw"end",pos())
  | "empty" -> (Keyw"empty",pos())
  | "true" -> (Keyw"true",pos())
  | "false" -> (Keyw"false",pos())
  | "try" -> (Keyw"try",pos())
  | "catch" -> (Keyw"catch",pos())
  | "break" -> (Keyw"break",pos())
  | "label" -> (Keyw"label",pos())
  | ident -> (Ident (Sedlexing.Latin1.lexeme buf),pos())
  | Plus (ws|comment) -> rawtoken buf
  | eof -> (EOF,pos())
  | _ -> Fmt.(failwithf "Unexpected character: unread %a" Dump.string (readn 4 buf))

module Unescape = struct

let float ?(json=false) s =
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  match%sedlex lb with
    json_number, eof ->
    float_of_string (Sedlexing.Latin1.lexeme lb)
  | _ ->
    if json then
      failwith "convert_float: not a JSON float"
    else
      float_of_string s

let is_high_surrogate i =
  0xD800 <= i && i <= 0xDBFF

let is_low_surrogate i =
  0xDC00 <= i && i <= 0xDFFF

let code_of_surrogate_pair i j =
  let high10 = i - 0xD800 in
  let low10 = j - 0xDC00 in
  0x10000 + ((high10 lsl 10) lor low10)

let jsonstring s =
  let buf = Buffer.create (String.length s) in
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  let rec unrec0 () =
    match%sedlex lb with
      "\"" -> unrec1 ()
    | _ -> failwith "unquote_jsonstring: unexpected character"

  and unrec1 () =
    match%sedlex lb with
      Plus json_unescaped ->
      Buffer.add_string buf (Sedlexing.Latin1.lexeme lb) ;
      unrec1 ()
    | "\\", '"' -> Buffer.add_char buf '"' ; unrec1 ()
    | "\\", '\\' -> Buffer.add_char buf '\\' ; unrec1 ()
    | "\\", '/' -> Buffer.add_char buf '/' ; unrec1 ()
    | "\\", 'b' -> Buffer.add_char buf '\b' ; unrec1 ()
    | "\\", 'f' -> Buffer.add_char buf '\x0c' ; unrec1 ()
    | "\\", 'n' -> Buffer.add_char buf '\n' ; unrec1 ()
    | "\\", 'r' -> Buffer.add_char buf '\r' ; unrec1 ()
    | "\\", 't' -> Buffer.add_char buf '\t' ; unrec1 ()
    | "\\", 'u', Rep(hexdigit,4) ->
      let s = Sedlexing.Latin1.sub_lexeme lb 2 4 in
      let n = int_of_string ("0x"^s) in
      if Uchar.is_valid n then begin
        Buffer.add_utf_8_uchar buf (Uchar.of_int n) ;
        unrec1 ()
      end
      else if is_high_surrogate n then
          unrec2 n
      else begin
        Buffer.add_utf_8_uchar buf (Uchar.unsafe_of_int n) ;
        unrec1 ()
      end

    | '"' ->
      Buffer.contents buf

    | _ -> failwith "unquote_jsonstring: internal error"

and unrec2 hi =
  match%sedlex lb with
  | "\\", 'u', Rep(hexdigit,4) ->
    let s = Sedlexing.Latin1.sub_lexeme lb 2 4 in
    let lo = int_of_string ("0x"^s) in
    if is_low_surrogate lo then
      let u = code_of_surrogate_pair hi lo in
      Buffer.add_utf_8_uchar buf (Uchar.of_int u) ;
      unrec1 ()
    else Fmt.(failwithf "unquote_jsonstring: invalid unicode surrogates: (0x%04x, 0x%04x)" hi lo)

  | _ ->
    Fmt.(failwithf "unquote_jsonstring: missing low surrogate after hi: 0x%04x" hi)

  in unrec0 ()

end
