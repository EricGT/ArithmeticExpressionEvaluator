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

module ArithmeticExpressionEvaluator.InfixFactoredGrammar.Tests

open ArithmeticExpressionEvaluator.Lib
open ArithmeticExpressionEvaluator.Semantic
open ArithmeticExpressionEvaluator.PrefixLexer
open ArithmeticExpressionEvaluator.InfixFactoredGrammar

open NUnit.Framework

//#region "infixFactoredGrammar tests"

let private infixFactoredGrammarValues : (string * string list * token list * expr * int)[] = [|
    (
        // idx 0
        // InfixFactoredGrammar.infixFactoredGrammar.01
        "1+2",
        ["1"; "+"; "2"],
        [Integer "1"; Operator "+"; Integer "2"],
        Sum(Int 1,Int 2),
        3
    );
    (
        // idx 1
        // InfixFactoredGrammar.infixFactoredGrammar.02
        "2*3",
        ["2"; "*"; "3"],
        [Integer "2"; Operator "*"; Integer "3"],
        Product(Int 2,Int 3),
        6
    );
    (
        // idx 2
        // InfixFactoredGrammar.infixFactoredGrammar.03
        "3-1",
        ["3"; "-"; "1"],
        [Integer "3"; Operator "-"; Integer "1"],
        Difference(Int 3,Int 1),
        2
    );
    (
        // idx 3
        // InfixFactoredGrammar.infixFactoredGrammar.04
        "4/2",
        ["4"; "/"; "2"],
        [Integer "4"; Operator "/"; Integer "2"],
        Quotient(Int 4,Int 2),
        2
    );
    (
        // idx 4
        // InfixFactoredGrammar.infixFactoredGrammar.05
        "(1+2)+3",
        ["("; "1"; "+"; "2"; ")"; "+"; "3"],
        [OpenParen; Integer "1"; Operator "+"; Integer "2"; CloseParen; Operator "+"; Integer "3"],
        Sum (Sum (Int 1,Int 2),Int 3),
        6
    );
    (
        // idx 5
        // InfixFactoredGrammar.infixFactoredGrammar.06
        "1+(2+3)",
        ["1"; "+"; "("; "2"; "+"; "3"; ")"],
        [Integer "1"; Operator "+"; OpenParen; Integer "2"; Operator "+"; Integer "3"; CloseParen],
        Sum (Int 1,Sum (Int 2,Int 3)),
        6
    );
    (
        // idx 6
        // InfixFactoredGrammar.infixFactoredGrammar.07
        "(2*3)*4",
        ["("; "2"; "*"; "3"; ")"; "*"; "4"],
        [OpenParen; Integer "2"; Operator "*"; Integer "3"; CloseParen; Operator "*"; Integer "4"],
        Product (Product (Int 2,Int 3),Int 4),
        24
    );
    (
        // idx 7
        // InfixFactoredGrammar.infixFactoredGrammar.08
        "2*(3*4)",
        ["2"; "*"; "("; "3"; "*"; "4"; ")"],
        [Integer "2"; Operator "*"; OpenParen; Integer "3"; Operator "*"; Integer "4"; CloseParen],
        Product (Int 2, Product (Int 3,Int 4)),
        24
    );
    (
        // idx 8
        // InfixFactoredGrammar.infixFactoredGrammar.09
        "(5-2)-1",
        ["("; "5"; "-"; "2"; ")"; "-"; "1"],
        [OpenParen; Integer "5"; Operator "-"; Integer "2"; CloseParen; Operator "-"; Integer "1"],
        Difference (Difference (Int 5,Int 2),Int 1),
        2
    );
    (
        // idx 9
        // InfixFactoredGrammar.infixFactoredGrammar.010
        "5-(2-1)",
        ["5"; "-"; "("; "2"; "-"; "1"; ")"],
        [Integer "5"; Operator "-"; OpenParen; Integer "2"; Operator "-"; Integer "1"; CloseParen],
        Difference (Int 5, Difference (Int 2,Int 1)),
        4
    );
    (
        // idx 10
        // InfixFactoredGrammar.infixFactoredGrammar.011
        "(24/4)/2",
        ["("; "2"; "4"; "/"; "4"; ")"; "/"; "2"],
        [OpenParen; Integer "24"; Operator "/"; Integer "4"; CloseParen; Operator "/"; Integer "2"],
        Quotient (Quotient (Int 24,Int 4),Int 2),
        3
    );
    (
        // idx 11
        // InfixFactoredGrammar.infixFactoredGrammar.012
        "24/(4/2)",
        ["2"; "4"; "/"; "("; "4"; "/"; "2"; ")"],
        [Integer "24"; Operator "/"; OpenParen; Integer "4"; Operator "/"; Integer "2"; CloseParen],
        Quotient (Int 24, Quotient (Int 4,Int 2)),
        12
    );
    (
        // idx 12
        // InfixFactoredGrammar.infixFactoredGrammar.013
        "(2*3)+4",
        ["("; "2"; "*"; "3"; ")"; "+"; "4"],
        [OpenParen; Integer "2"; Operator "*"; Integer "3"; CloseParen; Operator "+"; Integer "4"],
        Sum (Product (Int 2,Int 3),Int 4),
        10
    );
    (
        // idx 13
        // InfixFactoredGrammar.infixFactoredGrammar.014
        "2*(3+4)",
        ["2"; "*"; "("; "3"; "+"; "4"; ")"],
        [Integer "2"; Operator "*"; OpenParen; Integer "3"; Operator "+"; Integer "4"; CloseParen],
        Product (Int 2, Sum (Int 3,Int 4)),
        14
    );
    (
        // idx 14
        // InfixFactoredGrammar.infixFactoredGrammar.015
        "(51-(2+3)*(4+5))/2",
        ["("; "5"; "1"; "-"; "("; "2"; "+"; "3"; ")"; "*"; "("; "4"; "+"; "5"; ")"; ")"; "/"; "2"],
        [OpenParen; Integer "51"; Operator "-"; OpenParen; Integer "2"; Operator "+";
         Integer "3"; CloseParen; Operator "*"; OpenParen; Integer "4"; Operator "+";
         Integer "5"; CloseParen; CloseParen; Operator "/"; Integer "2"],
        Quotient (Difference (Int 51,Product (Sum (Int 2,Int 3),Sum (Int 4,Int 5))),Int 2),
        3
    );
    (
        // idx 15
        // InfixFactoredGrammar.infixFactoredGrammar.16
        "1+2+3",
        ["1"; "+"; "2"; "+"; "3"],
        [Integer "1"; Operator "+"; Integer "2"; Operator "+"; Integer "3"],
        Sum (Sum (Int 1,Int 2),Int 3),
        6
    );
    (
        // idx 16
        // InfixFactoredGrammar.infixFactoredGrammar.17
        "5-2-1",
        ["5"; "-"; "2"; "-"; "1"],
        [Integer "5"; Operator "-"; Integer "2"; Operator "-"; Integer "1"],
        Difference (Difference (Int 5,Int 2),Int 1),
        2
    );
    (
        // idx 17
        // InfixFactoredGrammar.infixFactoredGrammar.18
        "2^3",
        ["2"; "^"; "3"],
        [Integer "2"; Operator "^"; Integer "3"],
        Power (Int 2, Int 3),
        8
    );
    (
        // idx 18
        // InfixFactoredGrammar.infixFactoredGrammar.19
        "(4^3)^2",
        ["("; "4"; "^"; "3"; ")"; "^"; "2"],
        [OpenParen; Integer "4"; Operator "^"; Integer "3"; CloseParen; Operator "^"; Integer "2"],
        Power (Power (Int 4, Int 3), Int 2),
        4096
    );
    (
        // idx 19
        // InfixFactoredGrammar.infixFactoredGrammar.20
        "4^(3^2)",
        ["4"; "^"; "("; "3"; "^"; "2"; ")";],
        [Integer "4"; Operator "^"; OpenParen; Integer "3"; Operator "^"; Integer "2"; CloseParen],
        Power (Int 4, Power (Int 3, Int 2)),
        262144
    );
    (
        // idx 20
        // InfixFactoredGrammar.infixFactoredGrammar.21
        "4^3^2",
        ["4"; "^"; "3"; "^"; "2"],
        [Integer "4"; Operator "^"; Integer "3"; Operator "^"; Integer "2"],
        Power (Int 4, Power (Int 3, Int 2)),
        262144
    );
    |]
[<Test>]
[<TestCase(0, TestName = "InfixFactoredGrammar.infixFactoredGrammar.01")>]
[<TestCase(1, TestName = "InfixFactoredGrammar.infixFactoredGrammar.02")>]
[<TestCase(2, TestName = "InfixFactoredGrammar.infixFactoredGrammar.03")>]
[<TestCase(3, TestName = "InfixFactoredGrammar.infixFactoredGrammar.04")>]
[<TestCase(4, TestName = "InfixFactoredGrammar.infixFactoredGrammar.05")>]
[<TestCase(5, TestName = "InfixFactoredGrammar.infixFactoredGrammar.06")>]
[<TestCase(6, TestName = "InfixFactoredGrammar.infixFactoredGrammar.07")>]
[<TestCase(7, TestName = "InfixFactoredGrammar.infixFactoredGrammar.08")>]
[<TestCase(8, TestName = "InfixFactoredGrammar.infixFactoredGrammar.09")>]
[<TestCase(9, TestName = "InfixFactoredGrammar.infixFactoredGrammar.010")>]
[<TestCase(10, TestName = "InfixFactoredGrammar.infixFactoredGrammar.011")>]
[<TestCase(11, TestName = "InfixFactoredGrammar.infixFactoredGrammar.012")>]
[<TestCase(12, TestName = "InfixFactoredGrammar.infixFactoredGrammar.013")>]
[<TestCase(13, TestName = "InfixFactoredGrammar.infixFactoredGrammar.014")>]
[<TestCase(14, TestName = "InfixFactoredGrammar.infixFactoredGrammar.015")>]
[<TestCase(15, TestName = "InfixFactoredGrammar.infixFactoredGrammar.016")>]
[<TestCase(16, TestName = "InfixFactoredGrammar.infixFactoredGrammar.017")>]
[<TestCase(17, TestName = "InfixFactoredGrammar.infixFactoredGrammar.018")>]
[<TestCase(18, TestName = "InfixFactoredGrammar.infixFactoredGrammar.019")>]
[<TestCase(19, TestName = "InfixFactoredGrammar.infixFactoredGrammar.020")>]
[<TestCase(20, TestName = "InfixFactoredGrammar.infixFactoredGrammar.021")>]

let ``function InfixFactoredGrammar.infixFactoredGrammar`` idx =
    let (externalForm, _, _, _, _) = infixFactoredGrammarValues.[idx]
    let (_, internalForm, _, _, _) = infixFactoredGrammarValues.[idx]
    let (_, _, tokenList, _, _) = infixFactoredGrammarValues.[idx]
    let (_, _, _, expr, _) = infixFactoredGrammarValues.[idx]
    let (_, _, _, _, result) = infixFactoredGrammarValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)
//    printfn "passed explode step"

    // Verify result of lexer
    let lexResult = ArithmeticExpressionEvaluator.PrefixLexer.prefixLex internalForm
//    printfn "internalForm: %A" tokenList
//    printfn "lex result: %A" lexResult
    Assert.AreEqual (lexResult, tokenList)
//    printfn "passed lex step"

    // Verify result of parser
    let parserResult = ArithmeticExpressionEvaluator.InfixFactoredGrammar.infixFactoredGrammar tokenList
//    printfn "expr: %A" expr
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