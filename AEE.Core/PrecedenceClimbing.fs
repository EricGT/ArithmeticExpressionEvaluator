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
    let rec expression (tokens : token list) (minPrec : int) : (expr * token list) =
        let (lhs,rest) = parser tokens
        match rest with
        | [] -> (lhs, rest)
        | _ ->
            let rec makeExpr tokens minPrec lhs : (expr * token list) =
                match tokens with
                | Operator Op::tokensTail ->
                    let prec = precedence.Item(Op)
                    match prec >= minPrec with
                    | true ->
                        let prec1 Op =
                            let prec = precedence.Item(Op)
                            let assoc = associtivity.Item(Op)
                            match assoc with
                            | Left -> prec + 1
                            | Right -> prec
                        let (rhs, tokensTail) = expression tokensTail (prec1 Op)
                        let branch =
                            match Op with
                            | "+" -> Sum(lhs, rhs)
                            | "-" -> Difference(lhs, rhs)
                            | "*" -> Product(lhs, rhs)
                            | "/" -> Quotient(lhs, rhs)
                            | "^" -> Power(lhs, rhs)
                            | _ -> raise Noparse
                        makeExpr tokensTail minPrec branch
                    | false -> (lhs, tokens)
                | CloseParen::tokensTail -> (lhs, tokens)
                | [] -> (lhs,[])
                | _ -> raise Noparse
            makeExpr rest minPrec lhs
    and parser (tokens : token list) : (expr * token list) =
        match tokens with
        | OpenParen::tokensTail ->
            let (expr,rest) = expression tokensTail 0
            match rest with
            | CloseParen::tokensTail -> (expr, tokensTail)
            | _ -> raise Noparse
        | Integer v::tokensTail ->
            let branch = Int (System.Int32.Parse(v))
            (branch, tokensTail)
        | _ -> raise Noparse
    let (expr, tokens) = expression tokens 0
    expr

//#endregion

