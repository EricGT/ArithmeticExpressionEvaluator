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

module ArithmeticExpressionEvaluator.ShuntingYard.Tests

open ArithmeticExpressionEvaluator.Lib
open ArithmeticExpressionEvaluator.Semantic
open ArithmeticExpressionEvaluator.PrefixLexer
open ArithmeticExpressionEvaluator.ShuntingYard

open NUnit.Framework

//#region "shuntingYard tests"

let private shuntingYardValues : (string * string list * token list * token list * token list * expr * int)[] = [|
    (
        // idx 0
        // ShuntingYard.shuntingyard.01
        // 1+2
        "1+2",
        ["1"; "+"; "2"],
        [Integer "1"; Operator "+"; Integer "2"],
        [Integer "1"; Integer "2"; Operator "+"],
        [Operator "+"; Integer "1"; Integer "2"],
        Sum(Int 1,Int 2),
        3
    );
    (
        // idx 1
        // ShuntingYard.shuntingyard.02
        // 2*3
        "2*3",
        ["2"; "*"; "3"],
        [Integer "2"; Operator "*"; Integer "3"],
        [Integer "2"; Integer "3"; Operator "*"],
        [Operator "*"; Integer "2"; Integer "3"],
        Product(Int 2,Int 3),
        6
    );
    (
        // idx 2
        // ShuntingYard.shuntingyard.03
        // 3-1
        "3-1",
        ["3"; "-"; "1"],
        [Integer "3"; Operator "-"; Integer "1"],
        [Integer "3"; Integer "1"; Operator "-"],
        [Operator "-"; Integer "3"; Integer "1"],
        Difference(Int 3,Int 1),
        2
    );
    (
        // idx 3
        // ShuntingYard.shuntingyard.04
        // 4/2
        "4/2",
        ["4"; "/"; "2"],
        [Integer "4"; Operator "/"; Integer "2"],
        [Integer "4"; Integer "2"; Operator "/"],
        [Operator "/"; Integer "4"; Integer "2"],
        Quotient(Int 4,Int 2),
        2
    );
    (
        // idx 4
        // ShuntingYard.shuntingyard.05
        // (1+2)+3
        "(1+2)+3",
        ["("; "1"; "+"; "2"; ")"; "+"; "3"],
        [OpenParen; Integer "1"; Operator "+"; Integer "2"; CloseParen; Operator "+"; Integer "3"],
        [Integer "1"; Integer "2"; Operator "+"; Integer "3"; Operator "+"],
        [Operator "+"; Operator "+"; Integer "1"; Integer "2"; Integer "3"],
        Sum (Sum (Int 1,Int 2),Int 3),
        6
    );
    (
        // idx 5
        // ShuntingYard.shuntingyard.06
        // 1+(2+3)
        "1+(2+3)",
        ["1"; "+"; "("; "2"; "+"; "3"; ")"],
        [Integer "1"; Operator "+"; OpenParen; Integer "2"; Operator "+"; Integer "3"; CloseParen],
        [Integer "1"; Integer "2"; Integer "3"; Operator "+"; Operator "+"],
        [Operator "+"; Integer "2"; Integer "3"; Operator "+"; Integer "1"],
        Sum (Int 1,Sum (Int 2,Int 3)),
        6
    );
    (
        // idx 6
        // ShuntingYard.shuntingyard.07
        // (2*3)*4
        "(2*3)*4",
        ["("; "2"; "*"; "3"; ")"; "*"; "4"],
        [OpenParen; Integer "2"; Operator "*"; Integer "3"; CloseParen; Operator "*"; Integer "4"],
        [Integer "2"; Integer "3"; Operator "*"; Integer "4"; Operator "*"],
        [Integer "2"; Integer "3"; Operator "*"; Integer "4"; Operator "*"],
        Product (Product (Int 2,Int 3),Int 4),
        24
    );
    (
        // idx 7
        // ShuntingYard.shuntingyard.08
        // 2*(3*4)
        "2*(3*4)",
        ["2"; "*"; "("; "3"; "*"; "4"; ")"],
        [Integer "2"; Operator "*"; OpenParen; Integer "3"; Operator "*"; Integer "4"; CloseParen],
        [Integer "2"; Integer "3"; Integer "4"; Operator "*"; Operator "*"],
        [Integer "2"; Integer "3"; Integer "4"; Operator "*"; Operator "*"],
        Product (Int 2, Product (Int 3,Int 4)),
        24
    );
    (
        // idx 8
        // ShuntingYard.shuntingyard.09
        // (5-2)-1
        "(5-2)-1",
        ["("; "5"; "-"; "2"; ")"; "-"; "1"],
        [OpenParen; Integer "5"; Operator "-"; Integer "2"; CloseParen; Operator "-"; Integer "1"],
        [Integer "5"; Integer "2"; Operator "-"; Integer "1"; Operator "-"],
        [Integer "5"; Integer "2"; Operator "-"; Integer "1"; Operator "-"],
        Difference (Difference (Int 5,Int 2),Int 1),
        2
    );
    (
        // idx 9
        // ShuntingYard.shuntingyard.010
        // 5-(2-1)
        "5-(2-1)",
        ["5"; "-"; "("; "2"; "-"; "1"; ")"],
        [Integer "5"; Operator "-"; OpenParen; Integer "2"; Operator "-"; Integer "1"; CloseParen],
        [Integer "5"; Integer "2"; Integer "1"; Operator "-"; Operator "-"],
        [Integer "5"; Integer "2"; Integer "1"; Operator "-"; Operator "-"],
        Difference (Int 5, Difference (Int 2,Int 1)),
        4
    );
    (
        // idx 10
        // ShuntingYard.shuntingyard.011
        // (24/4)/2
        "(24/4)/2",
        ["("; "2"; "4"; "/"; "4"; ")"; "/"; "2"],
        [OpenParen; Integer "24"; Operator "/"; Integer "4"; CloseParen; Operator "/"; Integer "2"],
        [Integer "24"; Integer "4"; Operator "/"; Integer "2"; Operator "/"],
        [Integer "24"; Integer "4"; Operator "/"; Integer "2"; Operator "/"],
        Quotient (Quotient (Int 24,Int 4),Int 2),
        3
    );
    (
        // idx 11
        // ShuntingYard.shuntingyard.012
        // 24/(4/2)
        "24/(4/2)",
        ["2"; "4"; "/"; "("; "4"; "/"; "2"; ")"],
        [Integer "24"; Operator "/"; OpenParen; Integer "4"; Operator "/"; Integer "2"; CloseParen],
        [Integer "24"; Integer "4"; Integer "2"; Operator "/"; Operator "/"],
        [Integer "24"; Integer "4"; Integer "2"; Operator "/"; Operator "/"],
        Quotient (Int 24, Quotient (Int 4,Int 2)),
        12
    );
    (
        // idx 12
        // ShuntingYard.shuntingyard.013
        // (2*3)+4
        "(2*3)+4",
        ["("; "2"; "*"; "3"; ")"; "+"; "4"],
        [OpenParen; Integer "2"; Operator "*"; Integer "3"; CloseParen; Operator "+"; Integer "4"],
        [Integer "2"; Integer "3"; Operator "*"; Integer "4"; Operator "+"],
        [Integer "2"; Integer "3"; Operator "*"; Integer "4"; Operator "+"],
        Sum (Product (Int 2,Int 3),Int 4),
        10
    );
    (
        // idx 13
        // ShuntingYard.shuntingyard.014
        // 2*(3+4)
        "2*(3+4)",
        ["2"; "*"; "("; "3"; "+"; "4"; ")"],
        [Integer "2"; Operator "*"; OpenParen; Integer "3"; Operator "+"; Integer "4"; CloseParen],
        [Integer "2"; Integer "3"; Integer "4"; Operator "+"; Operator "*"],
        [Integer "2"; Integer "3"; Integer "4"; Operator "+"; Operator "*"],
        Product (Int 2, Sum (Int 3,Int 4)),
        14
    );
    (
        // idx 14
        // ShuntingYard.shuntingyard.015
        // (51-(2+3)*(4+5))/2
        "(51-(2+3)*(4+5))/2",
        ["("; "5"; "1"; "-"; "("; "2"; "+"; "3"; ")"; "*"; "("; "4"; "+"; "5"; ")"; ")"; "/"; "2"],
        [OpenParen; Integer "51"; Operator "-"; OpenParen; Integer "2"; Operator "+";
         Integer "3"; CloseParen; Operator "*"; OpenParen; Integer "4"; Operator "+";
         Integer "5"; CloseParen; CloseParen; Operator "/"; Integer "2"],
        [Integer "51"; Integer "2"; Integer "3"; Operator "+"; Integer "4"; Integer "5"; Operator "+"; Operator "*"; Operator "-"; Integer "2"; Operator "/"],
        [Integer "51"; Integer "2"; Integer "3"; Operator "+"; Integer "4"; Integer "5"; Operator "+"; Operator "*"; Operator "-"; Integer "2"; Operator "/"],
        Quotient (Difference (Int 51,Product (Sum (Int 2,Int 3),Sum (Int 4,Int 5))),Int 2),
        3
    );
    |]
[<Test>]
[<TestCase(0, TestName = "ShuntingYard.shuntingyard.01")>]
[<TestCase(1, TestName = "ShuntingYard.shuntingyard.02")>]
[<TestCase(2, TestName = "ShuntingYard.shuntingyard.03")>]
[<TestCase(3, TestName = "ShuntingYard.shuntingyard.04")>]
[<TestCase(4, TestName = "ShuntingYard.shuntingyard.05")>]
[<TestCase(5, TestName = "ShuntingYard.shuntingyard.06")>]
[<TestCase(6, TestName = "ShuntingYard.shuntingyard.07")>]
[<TestCase(7, TestName = "ShuntingYard.shuntingyard.08")>]
[<TestCase(8, TestName = "ShuntingYard.shuntingyard.09")>]
[<TestCase(9, TestName = "ShuntingYard.shuntingyard.010")>]
[<TestCase(10, TestName = "ShuntingYard.shuntingyard.011")>]
[<TestCase(11, TestName = "ShuntingYard.shuntingyard.012")>]
[<TestCase(12, TestName = "ShuntingYard.shuntingyard.013")>]
[<TestCase(13, TestName = "ShuntingYard.shuntingyard.014")>]
[<TestCase(14, TestName = "ShuntingYard.shuntingyard.015")>]

let ``function Infix.parser`` idx =
    let (externalForm, _, _, _, _, _, _) = shuntingYardValues.[idx]
    let (_, internalForm, _, _, _, _, _) = shuntingYardValues.[idx]
    let (_, _, infixTokenList, _, _, _, _) = shuntingYardValues.[idx]
    let (_, _, _, postfixTokenList, _, _, _) = shuntingYardValues.[idx]
    let (_, _, _, _, prefixTokenList, _, _) = shuntingYardValues.[idx]
    let (_, _, _, _, _, expr, _) = shuntingYardValues.[idx]
    let (_, _, _, _, _, _, result) = shuntingYardValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)
//    printfn "passed explode step"

    // Verify result of lexer
    let lexResult = ArithmeticExpressionEvaluator.ShuntingYard.infixLex convertedForm
//    printfn "internalForm: %A" infixTokenList
//    printfn "lex result: %A" lexResult
    Assert.AreEqual (lexResult, infixTokenList)
//    printfn "passed lex step"

    // Verify result of Shunting Yard Alogrithm to expression
    let shuntingYardAstResult = ArithmeticExpressionEvaluator.ShuntingYard.ShuntingYardAst lexResult
//    printfn "expr1: %A" expr1
//    printfn "Shunting Yard result: %A" shuntingYardResult
    Assert.AreEqual (shuntingYardAstResult, expr)
//    printfn "passed parser step"

    // Verify result of expression evaluator
    let evalResult = ArithmeticExpressionEvaluator.Semantic.eval shuntingYardAstResult
//    printfn "expected result: %A" result
//    printfn "function result: %A" evalResult
    Assert.AreEqual (evalResult, result)
//    printfn "passed eval step"

    // Verify result of Shunting Yard Alogrithm
    let shuntingYardResult = ArithmeticExpressionEvaluator.ShuntingYard.ShuntingYard lexResult
//    printfn "postfixTokenList: %A" postfixTokenList
//    printfn "Shunting Yard result: %A" shuntingYardResult
    Assert.AreEqual (shuntingYardResult, postfixTokenList)
//    printfn "passed Shunting Yard conversion step"

//    // Verify result of parser
    let evalResult = ArithmeticExpressionEvaluator.ShuntingYard.postfixEval shuntingYardResult
//    printfn "expected result: %A" result
//    printfn "function result: %A" evalResult
    Assert.AreEqual (evalResult, result)
//    printfn "passed postfix eval step"

//#endregion

