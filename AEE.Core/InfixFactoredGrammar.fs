//#region License
(*

Copyright 2013 Eric Taucher

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*)

//#endregion

module ArithmeticExpressionEvaluator.InfixFactoredGrammar

open ArithmeticExpressionEvaluator.Lib
open ArithmeticExpressionEvaluator.Semantic
open ArithmeticExpressionEvaluator.ParserCombinator
open ArithmeticExpressionEvaluator.PrefixLexer

//#region Infix Parser

let rec expr (l : token list) : expr * token list =
    let btyop n n' x y =
        match n' with
        | Operator "+" -> Sum(x,y)
        | Operator "-" -> Difference(x,y)
        | _ -> raise Noparse
    leftbin term ((a (Operator "+")) <|> (a (Operator "-")))  (btyop "expr") "type" l
and term (l : token list) : expr * token list =
    let btyop n n' x y =
        match n' with
        | Operator "*" -> Product(x,y)
        | Operator "/" -> Quotient(x,y)
        | _ -> raise Noparse
    leftbin factor ((a (Operator "*")) <|> (a (Operator "/")))  (btyop "term") "type" l
and factor (l : token list) : expr * token list =
    let btyop n n' x y =
        match n' with
        | Operator "^" -> Power(x,y)
        | _ -> raise Noparse
    rightbin ``base`` (a (Operator "^")) (btyop "factor") "type" l
and ``base`` (l : token list) : expr * token list  =
    let int l =
        match l with
        | (Integer x)::tl ->
            let intValue = System.Int32.Parse(x)
            (Int intValue, tl)
        | _ -> raise Noparse
    let parenExpr =
        let parser = a OpenParen .>>. expr .>>. a CloseParen
        let mk = (fun ((_,e),_) -> e)
        parser |>> mk
    (int <|> parenExpr) l
let infixFactoredGrammar l =
    fst (expr l)

//#endregion
