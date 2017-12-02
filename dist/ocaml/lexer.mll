{
  open Parser
  let line = ref 0
  let init () = line := 0
  let conv = function
    | "add" -> ADD
    | "sub" -> SUB
    | "mul" -> MUL
    | "div" -> DIV
    | "ret" -> RET
    | "call" -> CALL
    | "enter" -> ENTER
    | "print" -> PRINT
    | v -> ID v
}
let upper = ['A'-'Z'] | '\xce' ['\x91' - '\xa9']
let lower = ['a'-'z'] | '\xce' ['\xb1' - '\xbf'] | '\xcf' ['\x80' - '\x89']
let digit = ['0'-'9']
let id = (lower|upper|'_') (lower|upper|digit|'_')*
let nonendl = [^'\n']*
let int = digit+
let ln = ('\r' '\n') | '\r' | '\n'
rule token = parse
  | "#" nonendl { token lexbuf }
  | [' ' '\t']  { token lexbuf }
  | ':'         { COLON }
  | ln          { incr line; EOL (!line) }
  | id  as s    { conv s }
  | int as s    { INT (int_of_string s) }
  | eof         { incr line; EOF (!line) }
  | _  { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }