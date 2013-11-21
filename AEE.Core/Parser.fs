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

module ArithmeticExpressionEvaluator.Parser

open ArithmeticExpressionEvaluator.Lib
open ArithmeticExpressionEvaluator.Semantic
open ArithmeticExpressionEvaluator.ParserCombinator
open ArithmeticExpressionEvaluator.Lexer

// BNF
// expr =
//   | int
//   | + ( expr , expr )
//   | * ( expr , expr )
//   | - ( expr , expr )
//   | / ( expr , expr )

let rec intExpr (l : token list) : (expr * token list) =
    match l with
    | (Integer x)::tl -> 
        let intValue = System.Int32.Parse(x)
        (Int intValue, tl)
    | _ -> raise Noparse

and sumExpr (l : token list) : (expr * token list) =
    let parser = a (Operator "+") .>>. a OpenParen .>>. exprs .>>. a Comma .>>. exprs .>>. a CloseParen
    let mk = (fun (((((_,_),left),_),right),_) -> Sum(left,right))
    (parser |>> mk) l

and prodExpr (l : token list) : (expr * token list) =
    let parser = a (Operator "*") .>>. a OpenParen .>>. exprs .>>. a Comma .>>. exprs .>>. a CloseParen
    let mk = (fun (((((_,_),left),_),right),_) -> Product(left,right))
    (parser |>> mk) l

and diffExpr (l : token list) : (expr * token list) =
    let parser = a (Operator "-") .>>. a OpenParen .>>. exprs .>>. a Comma .>>. exprs .>>. a CloseParen
    let mk = (fun (((((_,_),left),_),right),_) -> Difference(left,right))
    (parser |>> mk) l

and quotExpr (l : token list) : (expr * token list) =
    let parser = a (Operator "/") .>>. a OpenParen .>>. exprs .>>. a Comma .>>. exprs .>>. a CloseParen
    let mk = (fun (((((_,_),left),_),right),_) -> Quotient(left,right))
    (parser |>> mk) l

and exprs =
    intExpr <|>
    sumExpr <|>
    prodExpr <|>
    diffExpr <|>
    quotExpr 

let prefixParser l =
    fst (exprs l)
