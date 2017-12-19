(* An astonishingly primitive lexer. *)

signature LEXER =
sig
  datatype token = SLASH | DOT | DOLLAR | ID of string | PIPE
  val lex : string -> token list
end

structure Lexer : LEXER =
struct
  datatype token = SLASH | DOT | DOLLAR | ID of string | PIPE

  exception LexError of string

  fun lexpipe [] = raise LexError "Half-formed pipe. Reserved token | without >"
    | lexpipe ((#">")::xs) = xs
    | lexpipe _ = raise LexError "Half-formed pipe. Reserved token | without >"

  fun lexid [] = ([], [])
    | lexid (x::xs) =
      case x of
          (#"\\" | #"$" | #"." | #"|") => ([], x::xs)
        | #" " => ([], xs)
        | _ => (let
                 val (str, xs') = lexid xs
               in
                 (x::str, xs')
               end)

  fun lexList [] = []
    | lexList [x] =
      (case x of
           #"\\" => [SLASH]
         | #"$" => [DOLLAR]
         | #"." => [DOT]
         | #" " => []
         | #"|" => raise LexError "Half-formed pipe at EOF."
         | _ => [ID (implode [x])])
    | lexList (x::xs) =
      case x of
          #" " => lexList xs
        | #"\\" => SLASH :: (lexList xs)
        | #"$" => DOLLAR :: (lexList xs)
        | #"." => DOT :: (lexList xs)
        | #"|" => (let
                    val xs' = lexpipe xs
                  in
                    PIPE :: (lexList xs')
                  end)
        | _ =>  (let
                  val (str, xs') = lexid (x::xs)
                in
                  case length str of
                      0 => raise LexError "Unexpected end of identifer. Identifier cannot contain \\, $, ., or |."
                    | _ => (ID (implode str)) :: (lexList xs')
                end)

  fun lex s = lexList (explode s) handle LexError s => raise Fail s
end
