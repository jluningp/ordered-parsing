signature PARSER =
sig
  datatype ast = APPOVER of ast * ast
               | APPUNDER of ast * ast
               | TOKEN of Lexer.token

  val parse : string -> ast
  val printAst : ast -> string
end

structure Parser : PARSER =
struct
  open Lexer

  datatype typ = TOVER of typ * typ
               | TUNDER of typ * typ
               | TARGS
               | TEXP
               | TID


  datatype ast = APPOVER of ast * ast
               | APPUNDER of ast * ast
               | TOKEN of token

  exception ParseError of (ast * typ) list option

  open Lexer

  fun fst (a, b) = a
  fun snd (a, b) = b

  fun printTyp t =
      case t of
          TOVER (t1, t2) => "OVER (" ^ (printTyp t1) ^ ", " ^ (printTyp t2) ^ ")"
        | TUNDER (t1, t2) => "UNDER (" ^ (printTyp t1) ^ ", " ^ (printTyp t2) ^ ")"
        | TARGS => "ARGS"
        | TEXP => "EXP"
        | TID => "ID"

  fun printAst t =
      case t of
          APPOVER (t1, t2) => "APPOVER (" ^ (printAst t1) ^ ", " ^ (printAst t2) ^ ")"
        | APPUNDER (t1, t2) => "APPUNDER (" ^ (printAst t1) ^ ", " ^ (printAst t2) ^ ")"
        | TOKEN t => case t of
                         SLASH => "\\"
                       | DOLLAR => "$"
                       | DOT => "."
                       | ID s => s
                       | PIPE => "|>"


  (* Assigns a type to each token. *)
  fun lexToType l =
      case l of
          SLASH => (TOKEN SLASH, TOVER (TARGS, TID))
        | DOT => (TOKEN DOT, TUNDER (TARGS, TOVER (TEXP, TEXP)))
        | ID s => (TOKEN (ID s), TID)
        | DOLLAR => (TOKEN DOLLAR, TOVER (TEXP, TID))
        | PIPE => (TOKEN PIPE, TOVER (TUNDER (TEXP, TEXP), TEXP))

  (* The next four functions make up a "Theorem Prover" that also happens to parse. It's a Theorem
     Prover because it takes the rules in a logic and tries to determine if they can be applied in such
     a way that we get a valid judgement (most of the time we want to get to the judgement "e true", but
     in this case we want the judgement e : TEXP). We apply the rules for A / B and A \ B anywhere we can
     find adjacent terms with the right type, until we hopefully get a single AST with type TEXP. *)

  fun over ((term1, TOVER (B, A)), (term2, A')) = if A = A'
                                                  then (APPOVER (term1, term2), B)
                                                  else raise ParseError NONE
    | over _ = raise ParseError NONE

  fun under ((term1, A'), (term2, TUNDER (A, B))) = if A = A'
                                                    then (APPUNDER (term1, term2), B)
                                                    else raise ParseError NONE
    | under _ = raise ParseError NONE


  fun find [] = []
    | find [x] = [x]
    | find (x::y::xs) =
      over (x, y) :: (find xs)
      handle ParseError _ =>
             under (x, y) :: xs
             handle ParseError _ =>
                    x :: (find (y::xs))

  fun prove L prev =
      if L = prev then raise ParseError (SOME L)
      else case L of
               [x] => x
             | _ => prove (find L) L


  fun parse s =
      let
        val tokens = lex s
        val withTypes = map lexToType tokens
      in
        fst (prove withTypes [])
        handle ParseError (SOME L) =>
               raise Fail ("Parse Error. ASTs do not reduce to exp: " ^
                           ListFormat.listToString printAst (map fst L))
      end
end
