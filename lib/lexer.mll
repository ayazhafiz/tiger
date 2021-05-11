{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  }

let depth = ref 0
}

let whitespace = [' ' '\t']+
let newline = '\n' | "\r\n"

let nat = ['0'-'9']+

let ident = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | whitespace    { read lexbuf }
  | newline       { next_line lexbuf; read lexbuf }

  | nat as b      { INT  (int_of_string b) }
  | '"'           { str (Buffer.create 128) lexbuf }

  | "array"       { ARRAY }
  | "break"       { BREAK }
  | "do"          { DO }
  | "else"        { ELSE }
  | "end"         { END }
  | "for"         { FOR }
  | "function"    { FUNCTION }
  | "if"          { IF }
  | "in"          { IN }
  | "let"         { LET }
  | "nil"         { NIL }
  | "of"          { OF }
  | "then"        { THEN }
  | "to"          { TO }
  | "type"        { TYPE }
  | "var"         { VAR }
  | "while"       { WHILE }

  | ident as id   { IDENT id }

  | ":="      { ASSIGN }
  | "<>"      { NEQ }
  | "<="      { LEQ }
  | ">="      { GEQ }
  | ","       { COMMA }
  | ":"       { COLON }
  | ";"       { SEMI }
  | "("       { LPAREN }
  | ")"       { RPAREN }
  | "["       { LBRACKET }
  | "]"       { RBRACKET }
  | "{"       { LBRACE }
  | "}"       { RBRACE }
  | "."       { DOT }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "/"       { DIVIDE }
  | "="       { EQ }
  | "<"       { LT }
  | ">"       { GT }
  | "&"       { AND }
  | "|"       { OR }

  | "/*"      { depth := 1; comment lexbuf }

  | eof       { EOF }

  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }

and comment = parse
  | eof { raise (SyntaxError "Unterminated comment") }
  | "*/"
    { depth := pred !depth; if !depth > 0 then comment lexbuf else read lexbuf }
  | "/*"
    { depth := succ !depth; comment lexbuf }
  | newline
    { next_line lexbuf; comment lexbuf }
  | _
    { comment lexbuf }

and str strbuf = parse
  | eof
    { raise (SyntaxError "Unterminated string") }
  | '"'
    { STRING (Buffer.contents strbuf |> Scanf.unescaped) }
  | newline
    { next_line lexbuf;
      Buffer.add_char strbuf '\n';
      str strbuf lexbuf }
  | '\\'
    { Option.iter (fun ch -> Buffer.add_char strbuf ch) (escaped lexbuf);
      str strbuf lexbuf }
  | "\\\"" as q
    { Buffer.add_string strbuf q;
      str strbuf lexbuf }
  | _ as c
    { Buffer.add_char strbuf c;
      str strbuf lexbuf }

and escaped = parse
  | eof { raise (SyntaxError "Unterminated escape sequence") }
  | 'n' { Option.some '\n' }
  | 't' { Option.some '\t' }
  | '"' { Option.some '"' }
  | '\\' { Option.some '\\' }
  | ['0'-'9']['0'-'9']['0'-'9'] as b
    { let n = int_of_string b in
      Option.some (Char.chr n) }
  | whitespace '\\'
    { Option.none }
  | _ { raise (SyntaxError "Invalid escape sequence") }
