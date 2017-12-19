This is an interpreter for WeirdAf Lambda Calculus, a variation of Lambda Calculus with
weird syntax that's a blend of Bash and OCaml. Now there's a sentence you don't write every day.

The purpose of this language is as an example of using the ideas of Lambek Calculus to parse a programming language. Why you'd want to do this is another question. Fun, maybe?

The syntax is as follows:

```
id := <anything with length greater than 0 that doesn't contain \, $, ., or |>
lam := \id.exp
exp := $id | lam | app
app := exp |> exp
```

Yes, variables are used with a $ and declared without $, just like in Bash. And yes, that is the
pipe from OCaml. It made writing the parsing rules easier.

```
Typing rules for parsing:

Token        Type
\            args / id
identifier   id
.            args \ (exp / exp)
$            exp / id
|>           (exp \ exp) / exp
```

Examples of expressions in WeirdAf Lambda Calculus:

```
\x.$x |> \x.$x
\x.\y.$x
```
