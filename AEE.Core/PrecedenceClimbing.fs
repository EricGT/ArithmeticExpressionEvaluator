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

module ArithmeticExpressionEvaluator.PrecedenceClimbing

open ArithmeticExpressionEvaluator.Lib
open ArithmeticExpressionEvaluator.Semantic
open ArithmeticExpressionEvaluator.ParserCombinator
open ArithmeticExpressionEvaluator.ShuntingYard

//#region Precedence Climbing for AST

// Coverts infix tokens to expr i.e. prefix AST

let precedenceClimbing (tokens : token list) : expr =
    let rec expression (inputTokens : token list) (minPrec : int) : (expr * token list) =
        let (lhs,rest) = parser inputTokens
        match rest with
        | [] -> (lhs, rest)
        | _ ->
            let rec makeExpr (inputTokens : token list) (minPrec : int) (lhs : expr) : (expr * token list) =
                match inputTokens with
                | Operator infixOp::tokensTail ->
                    let (prec : int) = precedence infixOp Infix
                    match prec >= minPrec with
                    | true ->
                        let prec1 Op =
                            let prec = precedence Op Infix
                            let assoc = associtivity Op Infix
                            match assoc with
                            | Left -> prec + 1
                            | Right -> prec
                        let (rhs, tokensTail) = expression tokensTail (prec1 infixOp)
                        let branch =
                            match infixOp with
                            | "+" -> Sum(lhs, rhs)
                            | "-" -> Difference(lhs, rhs)
                            | "*" -> Product(lhs, rhs)
                            | "/" -> Quotient(lhs, rhs)
                            | "^" -> Power(lhs, rhs)
                            | _ -> raise Noparse
                        makeExpr tokensTail minPrec branch
                    | false -> (lhs, inputTokens)
                | CloseParen::tokensTail -> (lhs, inputTokens)
                | [] -> (lhs,[])
                | _ -> raise Noparse
            let (expr,tokens) = makeExpr rest minPrec lhs
            (expr,tokens)
    and parser (inputTokens : token list) : (expr * token list) =
        let prs =
            match inputTokens with
            | Operator prefixOp::tokensTail when isPrefix prefixOp ->
                let (expr,rest) = parser tokensTail
                let expr = Neg(expr)
                (expr,rest)
            | OpenParen::tokensTail ->
                let (expr,rest) = expression tokensTail 0
                match rest with
                | CloseParen::tokensTail -> (expr,tokensTail)
                | _ -> raise Noparse
            | Integer v::tokensTail ->
                let branch = Int (System.Int32.Parse(v))
                (branch,tokensTail)
            | _ -> raise Noparse
        let (expr,tokens) = prs
        postfixParser expr tokens
    and postfixParser (expr : expr) (inputTokens : token list) : (expr * token list) =
        let pfp =
            match inputTokens with
            | Operator postfixOp::tokensTail when isPostfix postfixOp ->
                match postfixOp with
                | "!" ->
                    let expr = Fact(expr)
                    (expr, tokensTail)
                | _ -> raise Noparse
            | _ -> (expr, inputTokens)
        let (expr,tokens) = pfp
        (expr,tokens)

    let (expr, tokens) = expression tokens 0
    expr

//#endregion

