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

// BNF - infix
//
// expr =
//   | term + expr
//   | term - expr
//   | term
//   
// term =
//   | factor * term
//   | factor / term
//   | factor
//
// factor =
//    | int
//    | ( expr )

let rec expr (l : token list) =

    let sum =
//        printfn "trying sum"
        let parser = term .>>. a (Operator "+") .>>. expr 
        let mk = (fun ((left,_),right) -> Sum(left,right))
        parser |>> mk

    let difference =
//        printfn "trying difference"
        let parser = term .>>. a (Operator "-") .>>. expr 
        let mk = (fun ((left,_),right) -> Difference(left,right))
        parser |>> mk

    (sum <|>
     difference <|>
     term) l

and term (l : token list) =

    let product =
//        printfn "trying product"
        let parser = factor .>>. a (Operator "*") .>>. term 
        let mk = (fun ((left,_),right) -> Product(left,right))
        parser |>> mk

    let quotient =
//        printfn "trying quotient"
        let parser = factor .>>. a (Operator "/") .>>. term 
        let mk = (fun ((left,_),right) -> Quotient(left,right))
        parser |>> mk

    (product <|>
     quotient <|>
     factor) l

and factor (l : token list) =

    let int l =
//        printfn "trying int"
        match l with
        | (Integer x)::tl -> 
            let intValue = System.Int32.Parse(x)
            (Int intValue, tl)
        | _ -> raise Noparse

    let parenExpr =
//        printfn "trying paren"
        let parser = a OpenParen .>>. expr .>>. a CloseParen 
        let mk = (fun ((_,e),_) -> e)
        parser |>> mk

    (int <|>
     parenExpr) l

let infixFactoredGrammar l =
    fst (expr l) 
    
//#endregion
