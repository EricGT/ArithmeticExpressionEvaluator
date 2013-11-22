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

module ArithmeticExpressionEvaluator.Infix.Tests

open ArithmeticExpressionEvaluator.Lib
open ArithmeticExpressionEvaluator.Semantic
open ArithmeticExpressionEvaluator.Lexer
open ArithmeticExpressionEvaluator.Infix

open NUnit.Framework

//#region "prefixparser tests"

let private prefixparserValues : (string * string list * token list * expr * int)[] = [|
    (
        // idx 0
        // Infix.parser.01
        // 1+2
        "1+2",
        ["1"; "+"; "2"],
        [Integer "1"; Operator "+"; Integer "2"],
        Sum(Int 1,Int 2),
        3
    );
    (
        // idx 1
        // Infix.parser.02
        // 2*3
        "2*3",
        ["2"; "*"; "3"],
        [Integer "2"; Operator "*"; Integer "3"],
        Product(Int 2,Int 3),
        6
    );
    (
        // idx 2
        // Infix.parser.03
        // 3-1
        "3-1",
        ["3"; "-"; "1"],
        [Integer "3"; Operator "-"; Integer "1"],
        Difference(Int 3,Int 1),
        2
    );
    (
        // idx 3
        // Infix.parser.04
        // 4/2
        "4/2",
        ["4"; "/"; "2"],
        [Integer "4"; Operator "/"; Integer "2"],
        Quotient(Int 4,Int 2),
        2
    );
    (
        // idx 4
        // Infix.parser.05
        // (1+2)+3
        "(1+2)+3",
        ["("; "1"; "+"; "2"; ")"; "+"; "3"],
        [OpenParen; Integer "1"; Operator "+"; Integer "2"; CloseParen; Operator "+"; Integer "3"],
        Sum (Sum (Int 1,Int 2),Int 3),
        6
    );
    (
        // idx 5
        // Infix.parser.06
        // 1+(2+3)
        "1+(2+3)",
        ["1"; "+"; "("; "2"; "+"; "3"; ")"],
        [Integer "1"; Operator "+"; OpenParen; Integer "2"; Operator "+"; Integer "3"; CloseParen],
        Sum (Int 1,Sum (Int 2,Int 3)),
        6
    );
    (
        // idx 6
        // Infix.parser.07
        // (2*3)*4
        "(2*3)*4",
        ["("; "2"; "*"; "3"; ")"; "*"; "4"],
        [OpenParen; Integer "2"; Operator "*"; Integer "3"; CloseParen; Operator "*"; Integer "4"],
        Product (Product (Int 2,Int 3),Int 4),
        24
    );
    (
        // idx 7
        // Infix.parser.08
        // 2*(3*4)
        "2*(3*4)",
        ["2"; "*"; "("; "3"; "*"; "4"; ")"],
        [Integer "2"; Operator "*"; OpenParen; Integer "3"; Operator "*"; Integer "4"; CloseParen],
        Product (Int 2, Product (Int 3,Int 4)),
        24
    );
    (
        // idx 8
        // Infix.parser.09
        // (5-2)-1
        "(5-2)-1",
        ["("; "5"; "-"; "2"; ")"; "-"; "1"],
        [OpenParen; Integer "5"; Operator "-"; Integer "2"; CloseParen; Operator "-"; Integer "1"],
        Difference (Difference (Int 5,Int 2),Int 1),
        2
    );
    (
        // idx 9
        // Infix.parser.010
        // 5-(2-1)
        "5-(2-1)",
        ["5"; "-"; "("; "2"; "-"; "1"; ")"],
        [Integer "5"; Operator "-"; OpenParen; Integer "2"; Operator "-"; Integer "1"; CloseParen],
        Difference (Int 5, Difference (Int 2,Int 1)),
        4
    );
    (
        // idx 10
        // Infix.parser.011
        // (24/4)/2
        "(24/4)/2",
        ["("; "2"; "4"; "/"; "4"; ")"; "/"; "2"],
        [OpenParen; Integer "24"; Operator "/"; Integer "4"; CloseParen; Operator "/"; Integer "2"],
        Quotient (Quotient (Int 24,Int 4),Int 2),
        3
    );
    (
        // idx 11
        // Infix.parser.012
        // 24/(4/2)
        "24/(4/2)",
        ["2"; "4"; "/"; "("; "4"; "/"; "2"; ")"],
        [Integer "24"; Operator "/"; OpenParen; Integer "4"; Operator "/"; Integer "2"; CloseParen],
        Quotient (Int 24, Quotient (Int 4,Int 2)),
        12
    );
    (
        // idx 12
        // Infix.parser.013
        // (2*3)+4
        "(2*3)+4",
        ["("; "2"; "*"; "3"; ")"; "+"; "4"],
        [OpenParen; Integer "2"; Operator "*"; Integer "3"; CloseParen; Operator "+"; Integer "4"],
        Sum (Product (Int 2,Int 3),Int 4),
        10
    );
    (
        // idx 13
        // Infix.parser.014
        // 2*(3+4)
        "2*(3+4)",
        ["2"; "*"; "("; "3"; "+"; "4"; ")"],
        [Integer "2"; Operator "*"; OpenParen; Integer "3"; Operator "+"; Integer "4"; CloseParen],
        Product (Int 2, Sum (Int 3,Int 4)),
        14
    );
    (
        // idx 14
        // Infix.parser.015
        // (51-(2+3)*(4+5))/2
        "(51-(2+3)*(4+5))/2",
        ["("; "5"; "1"; "-"; "("; "2"; "+"; "3"; ")"; "*"; "("; "4"; "+"; "5"; ")"; ")"; "/"; "2"],
        [OpenParen; Integer "51"; Operator "-"; OpenParen; Integer "2"; Operator "+";
         Integer "3"; CloseParen; Operator "*"; OpenParen; Integer "4"; Operator "+";
         Integer "5"; CloseParen; CloseParen; Operator "/"; Integer "2"],
        Quotient (Difference (Int 51,Product (Sum (Int 2,Int 3),Sum (Int 4,Int 5))),Int 2),
        3
    );
    |]
[<Test>]
[<TestCase(0, TestName = "Infix.parser.01")>]
[<TestCase(1, TestName = "Infix.parser.02")>]
[<TestCase(2, TestName = "Infix.parser.03")>]
[<TestCase(3, TestName = "Infix.parser.04")>]
[<TestCase(4, TestName = "Infix.parser.05")>]
[<TestCase(5, TestName = "Infix.parser.06")>]
[<TestCase(6, TestName = "Infix.parser.07")>]
[<TestCase(7, TestName = "Infix.parser.08")>]
[<TestCase(8, TestName = "Infix.parser.09")>]
[<TestCase(9, TestName = "Infix.parser.010")>]
[<TestCase(10, TestName = "Infix.parser.011")>]
[<TestCase(11, TestName = "Infix.parser.012")>]
[<TestCase(12, TestName = "Infix.parser.013")>]
[<TestCase(13, TestName = "Infix.parser.014")>]
[<TestCase(14, TestName = "Infix.parser.015")>]

let ``function Infix.parser`` idx =
    let (externalForm, _, _, _, _) = prefixparserValues.[idx]
    let (_, internalForm, _, _, _) = prefixparserValues.[idx]
    let (_, _, tokenList, _, _) = prefixparserValues.[idx]
    let (_, _, _, expr, _) = prefixparserValues.[idx]
    let (_, _, _, _, result) = prefixparserValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)
//    printfn "passed explode step"

    // Verify result of lexer
    let lexResult = ArithmeticExpressionEvaluator.Lexer.lex internalForm
//    printfn "internalForm: %A" tokenList
//    printfn "lex result: %A" lexResult
    Assert.AreEqual (lexResult, tokenList)
//    printfn "passed lex step"

    // Verify result of parser
    let parserResult = ArithmeticExpressionEvaluator.Infix.infixParser tokenList
//    printfn "tokenList: %A" tokenList
//    printfn "parser result: %A" parserResult
    Assert.AreEqual (parserResult, expr)
//    printfn "passed parser step"

    // Verify result of expression evaluator
    let evalResult = ArithmeticExpressionEvaluator.Semantic.eval parserResult
//    printfn "expected result: %A" result
//    printfn "function result: %A" evalResult
    Assert.AreEqual (evalResult, result)
//    printfn "passed eval step"

//#endregion