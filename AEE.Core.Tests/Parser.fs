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

module ArithmeticExpressionEvaluator.Parser.Tests

open ArithmeticExpressionEvaluator.Semantic
open ArithmeticExpressionEvaluator.Lexer
open ArithmeticExpressionEvaluator.Parser

open NUnit.Framework

//#region "prefixParser tests"

let private prefixparserValues : (string * string list * token list * expr * int)[] = [|
    (
        // idx 0
        // Parser.prefixParser.01
        // 1+2
        "+(1,2)",
        ["+"; "("; "1"; ","; "2"; ")"],
        [Operator "+"; OpenParen; Integer "1"; Comma; Integer "2"; CloseParen],
        Sum(Int 1,Int 2),
        3
    );
    (
        // idx 1
        // Parser.prefixParser.02
        // 2*3
        "*(2,3)",
        ["*"; "("; "2"; ","; "3"; ")"],
        [Operator "*"; OpenParen; Integer "2"; Comma; Integer "3"; CloseParen],
        Product(Int 2,Int 3),
        6
    );
    (
        // idx 2
        // Parser.prefixParser.03
        // 3-1
        "-(3,1)",
        ["-"; "("; "3"; ","; "1"; ")"],
        [Operator "-"; OpenParen; Integer "3"; Comma; Integer "1"; CloseParen],
        Difference(Int 3,Int 1),
        2
    );
    (
        // idx 3
        // Parser.prefixParser.04
        // 4/2
        "/(4,2)",
        ["/"; "("; "4"; ","; "2"; ")"],
        [Operator "/"; OpenParen; Integer "4"; Comma; Integer "2"; CloseParen],
        Quotient(Int 4,Int 2),
        2
    );
    (
        // idx 4
        // Parser.prefixParser.05
        // (1+2)+3
        "+(+(1,2),3)",
        ["+"; "("; "+"; "("; "1"; ","; "2"; ")"; ","; "3"; ")"],
        [Operator "+"; OpenParen; Operator "+"; OpenParen; Integer "1"; Comma; Integer "2"; CloseParen; Comma; Integer "3"; CloseParen],
        Sum(Sum(Int 1,Int 2),Int 3),
        6
    );
    (
        // idx 5
        // Parser.prefixParser.06
        // 1+(2+3)
        "+(1,+(2,3))",
        ["+"; "("; "1"; ","; "+"; "("; "2"; ","; "3"; ")"; ")"],
        [Operator "+"; OpenParen; Integer "1"; Comma; Operator "+"; OpenParen; Integer "2"; Comma; Integer "3"; CloseParen;  CloseParen],
        Sum(Int 1,Sum(Int 2,Int 3)),
        6
    );
    (
        // idx 6
        // Parser.prefixParser.07
        // (2*3)*4
        "*(*(2,3),4)",
        ["*"; "("; "*"; "("; "2"; ","; "3"; ")"; ","; "4"; ")"],
        [Operator "*"; OpenParen; Operator "*"; OpenParen; Integer "2"; Comma; Integer "3"; CloseParen; Comma; Integer "4"; CloseParen],
        Product(Product(Int 2,Int 3),Int 4),
        24
    );
    (
        // idx 7
        // Parser.prefixParser.08
        // 2*(3*4)
        "*(2,*(3,4))",
        ["*"; "("; "2"; ","; "*"; "("; "3"; ","; "4"; ")"; ")"],
        [Operator "*"; OpenParen; Integer "2"; Comma; Operator "*"; OpenParen; Integer "3"; Comma; Integer "4"; CloseParen; CloseParen],
        Product(Int 2,Product(Int 3,Int 4)),
        24
    );
    (
        // idx 8
        // Parser.prefixParser.09
        // (5-2)-1
        "-(-(5,2),1)",
        ["-"; "("; "-"; "("; "5"; ","; "2"; ")"; ","; "1"; ")"],
        [Operator "-"; OpenParen; Operator "-"; OpenParen; Integer "5"; Comma; Integer "2"; CloseParen; Comma; Integer "1"; CloseParen],
        Difference(Difference(Int 5,Int 2),Int 1),
        2
    );
    (
        // idx 9
        // Parser.prefixParser.010
        // 5-(2-1)
        "-(5,-(2,1))",
        ["-"; "("; "5"; ","; "-"; "("; "2"; ","; "1"; ")"; ")"],
        [Operator "-"; OpenParen; Integer "5"; Comma; Operator "-"; OpenParen; Integer "2"; Comma; Integer "1"; CloseParen; CloseParen],
        Difference(Int 5,Difference(Int 2,Int 1)),
        4
    );
    (
        // idx 10
        // Parser.prefixParser.011
        // (24/4)/2
        "/(/(24,4),3)",
        ["/"; "("; "/"; "("; "2"; "4"; ","; "4"; ")"; ","; "3"; ")"],
        [Operator "/"; OpenParen; Operator "/"; OpenParen; Integer "24"; Comma; Integer "4"; CloseParen; Comma; Integer "3"; CloseParen],
        Quotient(Quotient(Int 24,Int 4),Int 3),
        2
    );
    (
        // idx 11
        // Parser.prefixParser.012
        // 24/(4/2)
        "/(24,/(4,2))",
        ["/"; "("; "2"; "4"; ","; "/"; "("; "4"; ","; "2"; ")"; ")"],
        [Operator "/"; OpenParen; Integer "24"; Comma; Operator "/"; OpenParen; Integer "4"; Comma; Integer "2"; CloseParen; CloseParen],
        Quotient(Int 24,Quotient(Int 4,Int 2)),
        12
    );
    (
        // idx 12
        // Parser.prefixParser.013
        // (2*3)+4
        "+(*(2,3),4)",
        ["+"; "("; "*"; "("; "2"; ","; "3"; ")"; ","; "4"; ")"],
        [Operator "+"; OpenParen; Operator "*"; OpenParen; Integer "2"; Comma; Integer "3"; CloseParen; Comma; Integer "4"; CloseParen],
        Sum (Product (Int 2,Int 3),Int 4),
        10
    );
    (
        // idx 13
        // Parser.prefixParser.014
        // 2*(3+4)
        "*(2,+(3,4))",
        ["*"; "("; "2"; ","; "+"; "("; "3"; ","; "4"; ")"; ")"],
        [Operator "*"; OpenParen; Integer "2"; Comma; Operator "+"; OpenParen; Integer "3"; Comma; Integer "4"; CloseParen; CloseParen],
        Product (Int 2,Sum (Int 3,Int 4)),
        14
    );
    |]
[<Test>]
[<TestCase(0, TestName = "Parser.prefixParser.01")>]
[<TestCase(1, TestName = "Parser.prefixParser.02")>]
[<TestCase(2, TestName = "Parser.prefixParser.03")>]
[<TestCase(3, TestName = "Parser.prefixParser.04")>]
[<TestCase(4, TestName = "Parser.prefixParser.05")>]
[<TestCase(5, TestName = "Parser.prefixParser.06")>]
[<TestCase(6, TestName = "Parser.prefixParser.07")>]
[<TestCase(7, TestName = "Parser.prefixParser.08")>]
[<TestCase(8, TestName = "Parser.prefixParser.09")>]
[<TestCase(9, TestName = "Parser.prefixParser.010")>]
[<TestCase(10, TestName = "Parser.prefixParser.011")>]
[<TestCase(11, TestName = "Parser.prefixParser.012")>]
[<TestCase(12, TestName = "Parser.prefixParser.013")>]
[<TestCase(13, TestName = "Parser.prefixParser.014")>]

let ``function Parser.prefixParser`` idx =
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

    // Verify result of prefixParser
    let parserResult = ArithmeticExpressionEvaluator.Parser.prefixParser tokenList
//    printfn "tokenList: %A" tokenList
//    printfn "parser result: %A" parserResult
    Assert.AreEqual (parserResult, expr)
//    printfn "passed prefixParser step"

    // Verify result of expression evaluator
    let evalResult = ArithmeticExpressionEvaluator.Semantic.eval parserResult
//    printfn "expected result: %A" result
//    printfn "function result: %A" evalResult
    Assert.AreEqual (evalResult, result)
//    printfn "passed eval step"

//#endregion