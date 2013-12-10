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

let private shuntingYardValues : (string * string list * token list * token list * expr * int)[] = [|
    (
        // idx 0
        // ShuntingYard.shuntingYard.01
        "1+2",
        ["1"; "+"; "2"],
        [Integer "1"; Operator "+"; Integer "2"],
        [Integer "1"; Integer "2"; Operator "+"],
        Sum(Int 1,Int 2),
        3
    );
    (
        // idx 1
        // ShuntingYard.shuntingYard.02
        "2*3",
        ["2"; "*"; "3"],
        [Integer "2"; Operator "*"; Integer "3"],
        [Integer "2"; Integer "3"; Operator "*"],
        Product(Int 2,Int 3),
        6
    );
    (
        // idx 2
        // ShuntingYard.shuntingYard.03
        "3-1",
        ["3"; "-"; "1"],
        [Integer "3"; Operator "-"; Integer "1"],
        [Integer "3"; Integer "1"; Operator "-"],
        Difference(Int 3,Int 1),
        2
    );
    (
        // idx 3
        // ShuntingYard.shuntingYard.04
        "4/2",
        ["4"; "/"; "2"],
        [Integer "4"; Operator "/"; Integer "2"],
        [Integer "4"; Integer "2"; Operator "/"],
        Quotient(Int 4,Int 2),
        2
    );
    (
        // idx 4
        // ShuntingYard.shuntingYard.05
        "(1+2)+3",
        ["("; "1"; "+"; "2"; ")"; "+"; "3"],
        [OpenParen; Integer "1"; Operator "+"; Integer "2"; CloseParen; Operator "+"; Integer "3"],
        [Integer "1"; Integer "2"; Operator "+"; Integer "3"; Operator "+"],
        Sum (Sum (Int 1,Int 2),Int 3),
        6
    );
    (
        // idx 5
        // ShuntingYard.shuntingYard.06
        "1+(2+3)",
        ["1"; "+"; "("; "2"; "+"; "3"; ")"],
        [Integer "1"; Operator "+"; OpenParen; Integer "2"; Operator "+"; Integer "3"; CloseParen],
        [Integer "1"; Integer "2"; Integer "3"; Operator "+"; Operator "+"],
        Sum (Int 1,Sum (Int 2,Int 3)),
        6
    );
    (
        // idx 6
        // ShuntingYard.shuntingYard.07
        "(2*3)*4",
        ["("; "2"; "*"; "3"; ")"; "*"; "4"],
        [OpenParen; Integer "2"; Operator "*"; Integer "3"; CloseParen; Operator "*"; Integer "4"],
        [Integer "2"; Integer "3"; Operator "*"; Integer "4"; Operator "*"],
        Product (Product (Int 2,Int 3),Int 4),
        24
    );
    (
        // idx 7
        // ShuntingYard.shuntingYard.08
        "2*(3*4)",
        ["2"; "*"; "("; "3"; "*"; "4"; ")"],
        [Integer "2"; Operator "*"; OpenParen; Integer "3"; Operator "*"; Integer "4"; CloseParen],
        [Integer "2"; Integer "3"; Integer "4"; Operator "*"; Operator "*"],
        Product (Int 2, Product (Int 3,Int 4)),
        24
    );
    (
        // idx 8
        // ShuntingYard.shuntingYard.09
        "(5-2)-1",
        ["("; "5"; "-"; "2"; ")"; "-"; "1"],
        [OpenParen; Integer "5"; Operator "-"; Integer "2"; CloseParen; Operator "-"; Integer "1"],
        [Integer "5"; Integer "2"; Operator "-"; Integer "1"; Operator "-"],
        Difference (Difference (Int 5,Int 2),Int 1),
        2
    );
    (
        // idx 9
        // ShuntingYard.shuntingYard.010
        "5-(2-1)",
        ["5"; "-"; "("; "2"; "-"; "1"; ")"],
        [Integer "5"; Operator "-"; OpenParen; Integer "2"; Operator "-"; Integer "1"; CloseParen],
        [Integer "5"; Integer "2"; Integer "1"; Operator "-"; Operator "-"],
        Difference (Int 5, Difference (Int 2,Int 1)),
        4
    );
    (
        // idx 10
        // ShuntingYard.shuntingYard.011
        "(24/4)/2",
        ["("; "2"; "4"; "/"; "4"; ")"; "/"; "2"],
        [OpenParen; Integer "24"; Operator "/"; Integer "4"; CloseParen; Operator "/"; Integer "2"],
        [Integer "24"; Integer "4"; Operator "/"; Integer "2"; Operator "/"],
        Quotient (Quotient (Int 24,Int 4),Int 2),
        3
    );
    (
        // idx 11
        // ShuntingYard.shuntingYard.012
        "24/(4/2)",
        ["2"; "4"; "/"; "("; "4"; "/"; "2"; ")"],
        [Integer "24"; Operator "/"; OpenParen; Integer "4"; Operator "/"; Integer "2"; CloseParen],
        [Integer "24"; Integer "4"; Integer "2"; Operator "/"; Operator "/"],
        Quotient (Int 24, Quotient (Int 4,Int 2)),
        12
    );
    (
        // idx 12
        // ShuntingYard.shuntingYard.013
        "(2*3)+4",
        ["("; "2"; "*"; "3"; ")"; "+"; "4"],
        [OpenParen; Integer "2"; Operator "*"; Integer "3"; CloseParen; Operator "+"; Integer "4"],
        [Integer "2"; Integer "3"; Operator "*"; Integer "4"; Operator "+"],
        Sum (Product (Int 2,Int 3),Int 4),
        10
    );
    (
        // idx 13
        // ShuntingYard.shuntingYard.014
        "2*(3+4)",
        ["2"; "*"; "("; "3"; "+"; "4"; ")"],
        [Integer "2"; Operator "*"; OpenParen; Integer "3"; Operator "+"; Integer "4"; CloseParen],
        [Integer "2"; Integer "3"; Integer "4"; Operator "+"; Operator "*"],
        Product (Int 2, Sum (Int 3,Int 4)),
        14
    );
    (
        // idx 14
        // ShuntingYard.shuntingYard.015
        "(51-(2+3)*(4+5))/2",
        ["("; "5"; "1"; "-"; "("; "2"; "+"; "3"; ")"; "*"; "("; "4"; "+"; "5"; ")"; ")"; "/"; "2"],
        [OpenParen; Integer "51"; Operator "-"; OpenParen; Integer "2"; Operator "+";
         Integer "3"; CloseParen; Operator "*"; OpenParen; Integer "4"; Operator "+";
         Integer "5"; CloseParen; CloseParen; Operator "/"; Integer "2"],
        [Integer "51"; Integer "2"; Integer "3"; Operator "+"; Integer "4"; Integer "5"; Operator "+"; Operator "*"; Operator "-"; Integer "2"; Operator "/"],
        Quotient (Difference (Int 51,Product (Sum (Int 2,Int 3),Sum (Int 4,Int 5))),Int 2),
        3
    );
    (
        // idx 15
        // ShuntingYard.shuntingYard.16
        "1+2+3",
        ["1"; "+"; "2"; "+"; "3"],
        [Integer "1"; Operator "+"; Integer "2"; Operator "+"; Integer "3"],
        [Integer "1"; Integer "2"; Operator "+"; Integer "3"; Operator "+"],
        Sum (Sum (Int 1,Int 2),Int 3),
        6
    );
    (
        // idx 16
        // ShuntingYard.shuntingYard.17
        "5-2-1",
        ["5"; "-"; "2"; "-"; "1"],
        [Integer "5"; Operator "-"; Integer "2"; Operator "-"; Integer "1"],
        [Integer "5"; Integer "2"; Operator "-"; Integer "1"; Operator "-"],
        Difference (Difference (Int 5,Int 2),Int 1),
        2
    );
    (
        // idx 17
        // ShuntingYard.shuntingYard.18
        "2^2",
        ["2"; "^"; "2"],
        [Integer "2"; Operator "^"; Integer "2"],
        [Integer "2"; Integer "2"; Operator "^"],
        Power (Int 2,Int 2),
        4
    );
    (
        // idx 18
        // ShuntingYard.shuntingYard.19
        "(2^3)^2",
        // (2^3)^2 = 8^2 = 64
        ["("; "2"; "^"; "3"; ")"; "^"; "2"],
        [OpenParen; Integer "2"; Operator "^"; Integer "3"; CloseParen; Operator "^"; Integer "2"],
        [Integer "2"; Integer "3"; Operator "^"; Integer "2"; Operator "^"],
        Power (Power (Int 2,Int 3),Int 2),
        64
    );
    (
        // idx 19
        // ShuntingYard.shuntingYard.20
        "2^(3^2)",
        // 2^(3^2) = 2^9 = 512
        ["2"; "^"; "("; "3"; "^"; "2"; ")"],
        [Integer "2"; Operator "^"; OpenParen; Integer "3"; Operator "^"; Integer "2"; CloseParen],
        [Integer "2"; Integer "3"; Integer "2"; Operator "^"; Operator "^"],
        Power(Int 2, Power (Int 3,Int 2)),
        512
    );
    (
        // idx 20
        // ShuntingYard.shuntingYard.21
        "2^3^2",
        // 2^(3^2) = 2^9 = 512
        ["2"; "^"; "3"; "^"; "2"],
        [Integer "2"; Operator "^"; Integer "3"; Operator "^"; Integer "2"],
        [Integer "2"; Integer "3"; Integer "2"; Operator "^"; Operator "^"],
        Power(Int 2, Power (Int 3,Int 2)),
        512
    );
    (
        // idx 21
        // ShuntingYard.shuntingYard.022
        "24/4/2",
        ["2"; "4"; "/"; "4"; "/"; "2"],
        [Integer "24"; Operator "/"; Integer "4"; Operator "/"; Integer "2"],
        [Integer "24"; Integer "4"; Operator "/"; Integer "2"; Operator "/"],
        Quotient (Quotient (Int 24,Int 4), Int 2),
        3
    );
    (
        // idx 22
        // ShuntingYard.shuntingYard.023
        "2*3*4",
        ["2"; "*";  "3"; "*"; "4"],
        [Integer "2"; Operator "*"; Integer "3"; Operator "*"; Integer "4"],
        [Integer "2"; Integer "3"; Operator "*"; Integer "4"; Operator "*"],
        Product (Product (Int 2, Int 3), Int 4),
        24
    );
    (
        // idx 23
        // ShuntingYard.shuntingYard.024
        "-1",
        ["-"; "1"],
        [Operator "-"; Integer "1"],
        [Integer "1"; PrefixOperator "-"],
        Neg (Int 1),
        -1
    );
    (
        // idx 24
        // ShuntingYard.shuntingYard.025
        "-1*2",
        ["-"; "1"; "*"; "2"],
        [Operator "-"; Integer "1"; Operator "*"; Integer "2"],
        [Integer "1"; PrefixOperator  "-"; Integer "2"; Operator "*"],
        Product(Neg (Int 1), Int 2),
        -2
    );
    (
        // idx 25
        // ShuntingYard.shuntingYard.026
        "2*-1",
        ["2"; "*"; "-"; "1"],
        [Integer "2"; Operator "*"; Operator "-"; Integer "1"],
        [Integer "2"; Integer "1"; PrefixOperator "-"; Operator "*"],
        Product(Int 2, Neg (Int 1)),
        -2
    );
    (
        // idx 26
        // ShuntingYard.shuntingYard.027
        "-(3*4)*2",
        ["-"; "("; "3"; "*"; "4"; ")"; "*"; "2"],
        [Operator "-"; OpenParen; Integer "3"; Operator "*"; Integer "4"; CloseParen; Operator "*"; Integer "2"],
        [Integer "3"; Integer "4"; Operator "*"; PrefixOperator "-"; Integer "2"; Operator "*"],
        Product(Neg (Product(Int 3, Int 4)), Int 2),
        -24
    );
    (
        // idx 27
        // ShuntingYard.shuntingYard.028
        "2*-(3*4)",
        ["2"; "*"; "-"; "("; "3"; "*"; "4"; ")"],
        [Integer "2"; Operator "*"; Operator "-"; OpenParen; Integer "3"; Operator "*"; Integer "4"; CloseParen],
        [Integer "2"; Integer "3"; Integer "4"; Operator "*"; PrefixOperator "-"; Operator "*"],
        Product(Int 2, Neg (Product(Int 3, Int 4))),
        -24
    );
    (
        // idx 28
        // ShuntingYard.shuntingYard.029
        "0!",
        ["0"; "!"],
        [Integer "0"; Operator "!"],
        [Integer "0"; Operator "!"],
        Fact(Int 0),
        1
    );
    (
        // idx 29
        // ShuntingYard.shuntingYard.30
        "1!",
        ["1"; "!"],
        [Integer "1"; Operator "!"],
        [Integer "1"; Operator "!"],
        Fact(Int 1),
        1
    );
    (
        // idx 30
        // ShuntingYard.shuntingYard.031
        "3!",
        ["3"; "!"],
        [Integer "3"; Operator "!"],
        [Integer "3"; Operator "!"],
        Fact(Int 3),
        6
    );
    (
        // idx 31
        // ShuntingYard.shuntingYard.032
        "(1+2)!",
        ["("; "1"; "+"; "2"; ")"; "!"],
        [OpenParen; Integer "1"; Operator "+"; Integer "2"; CloseParen; Operator "!"],
        [Integer "1"; Integer "2"; Operator "+"; Operator "!"],
        Fact (Sum (Int 1,Int 2)),
        6
    );
    (
        // idx 32
        // ShuntingYard.shuntingYard.033
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "3-",
        ["3"; "-"],
        [Integer "3"; Operator "-"],
        [Integer "3"; Operator "-"],
        Int -1, // dummy value
        -1 // dummy value
    );
    (
        // idx 33
        // ShuntingYard.shuntingYard.034
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "!3",
        ["!"; "3"],
        [Operator "!"; Integer "3"],
        [Operator "-"; Integer "1"],
        Int -1, // dummy value
        -1 // dummy value
    );
    (
        // idx 34
        // ShuntingYard.shuntingYard.035
        "3--1",
        ["3"; "-"; "-"; "1"],
        [Integer "3"; Operator "-"; Operator "-"; Integer "1"],
        [Integer "3"; Integer "1"; PrefixOperator "-"; Operator "-"],
        Difference( Int 3, Neg(Int 1)),
        4
    );
    (
        // idx 35
        // ShuntingYard.shuntingYard.036
        "1",
        ["1"],
        [Integer "1"],
        [Integer "1"],
        Int 1,
        1
    );
    (
        // idx 36
        // ShuntingYard.shuntingYard.037
        "(1)",
        ["("; "1"; ")"],
        [OpenParen; Integer "1"; CloseParen],
        [Integer "1"],
        Int 1,
        1
    );
    (
        // idx 37
        // ShuntingYard.shuntingYard.038
        "(5-(4-(3-(2-1))))",
        ["("; "5"; "-"; "("; "4"; "-"; "("; "3"; "-"; "("; "2"; "-"; "1"; ")"; ")"; ")"; ")"],
        [OpenParen; Integer "5"; Operator "-"; OpenParen; Integer "4"; Operator "-"; OpenParen; Integer "3"; Operator "-"; OpenParen; Integer "2"; Operator "-"; Integer "1"; CloseParen; CloseParen; CloseParen; CloseParen],
        [Integer "5"; Integer "4"; Integer "3"; Integer "2"; Integer "1"; Operator "-"; Operator "-"; Operator "-"; Operator "-"],
        Difference (Int 5,Difference (Int 4,Difference (Int 3,Difference (Int 2,Int 1)))),
        3
    );
    |]
[<Test>]
[<TestCase(0, TestName = "PrecedenceClimbing.shuntingYard.01")>]
[<TestCase(1, TestName = "PrecedenceClimbing.shuntingYard.02")>]
[<TestCase(2, TestName = "PrecedenceClimbing.shuntingYard.03")>]
[<TestCase(3, TestName = "PrecedenceClimbing.shuntingYard.04")>]
[<TestCase(4, TestName = "PrecedenceClimbing.shuntingYard.05")>]
[<TestCase(5, TestName = "PrecedenceClimbing.shuntingYard.06")>]
[<TestCase(6, TestName = "PrecedenceClimbing.shuntingYard.07")>]
[<TestCase(7, TestName = "PrecedenceClimbing.shuntingYard.08")>]
[<TestCase(8, TestName = "PrecedenceClimbing.shuntingYard.09")>]
[<TestCase(9, TestName = "PrecedenceClimbing.shuntingYard.010")>]
[<TestCase(10, TestName = "PrecedenceClimbing.shuntingYard.011")>]
[<TestCase(11, TestName = "PrecedenceClimbing.shuntingYard.012")>]
[<TestCase(12, TestName = "PrecedenceClimbing.shuntingYard.013")>]
[<TestCase(13, TestName = "PrecedenceClimbing.shuntingYard.014")>]
[<TestCase(14, TestName = "PrecedenceClimbing.shuntingYard.015")>]
[<TestCase(15, TestName = "PrecedenceClimbing.shuntingYard.016")>]
[<TestCase(16, TestName = "PrecedenceClimbing.shuntingYard.017")>]
[<TestCase(17, TestName = "PrecedenceClimbing.shuntingYard.018")>]
[<TestCase(18, TestName = "PrecedenceClimbing.shuntingYard.019")>]
[<TestCase(19, TestName = "PrecedenceClimbing.shuntingYard.020")>]
[<TestCase(20, TestName = "PrecedenceClimbing.shuntingYard.021")>]
[<TestCase(21, TestName = "PrecedenceClimbing.shuntingYard.022")>]
[<TestCase(22, TestName = "PrecedenceClimbing.shuntingYard.023")>]
[<TestCase(23, TestName = "PrecedenceClimbing.shuntingYard.024")>]
[<TestCase(24, TestName = "PrecedenceClimbing.shuntingYard.025")>]
[<TestCase(25, TestName = "PrecedenceClimbing.shuntingYard.026")>]
[<TestCase(26, TestName = "PrecedenceClimbing.shuntingYard.027")>]
[<TestCase(27, TestName = "PrecedenceClimbing.shuntingYard.028")>]
[<TestCase(28, TestName = "PrecedenceClimbing.shuntingYard.029")>]
[<TestCase(29, TestName = "PrecedenceClimbing.shuntingYard.030")>]
[<TestCase(30, TestName = "PrecedenceClimbing.shuntingYard.031")>]
[<TestCase(31, TestName = "PrecedenceClimbing.shuntingYard.032")>]
[<TestCase(32, TestName = "PrecedenceClimbing.shuntingYard.033", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(33, TestName = "PrecedenceClimbing.shuntingYard.034", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(34, TestName = "PrecedenceClimbing.shuntingYard.035")>]
[<TestCase(35, TestName = "PrecedenceClimbing.shuntingYard.036")>]
[<TestCase(36, TestName = "PrecedenceClimbing.shuntingYard.037")>]
[<TestCase(37, TestName = "PrecedenceClimbing.shuntingYard.038")>]

let ``function Infix.parser`` idx =
    let (externalForm, _, _, _, _, _) = shuntingYardValues.[idx]
    let (_, internalForm, _, _, _, _) = shuntingYardValues.[idx]
    let (_, _, infixTokenList, _, _, _) = shuntingYardValues.[idx]
    let (_, _, _, postfixTokenList, _, _) = shuntingYardValues.[idx]
    let (_, _, _, _, expr, _) = shuntingYardValues.[idx]
    let (_, _, _, _, _, result) = shuntingYardValues.[idx]

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

    // Verify result of Shunting Yard Alogrithm for coversion to postfix
    let shuntingYardResult = ArithmeticExpressionEvaluator.ShuntingYard.ShuntingYard lexResult
//    printfn "postfixTokenList: %A" postfixTokenList
//    printfn "Shunting Yard result: %A" shuntingYardResult
    Assert.AreEqual (shuntingYardResult, postfixTokenList)
//    printfn "passed Shunting Yard conversion to postfix step"

//    // Verify result of postfix eval
    let evalResult = ArithmeticExpressionEvaluator.ShuntingYard.postfixEval shuntingYardResult
//    printfn "expected result: %A" result
//    printfn "function result: %A" evalResult
    Assert.AreEqual (evalResult, result)
//    printfn "passed postfix eval step"

    // Verify result of Shunting Yard Alogrithm to AST
    let shuntingYardAstResult = ArithmeticExpressionEvaluator.ShuntingYard.ShuntingYardAst shuntingYardResult
//    printfn "expr: %A" expr
//    printfn "Shunting Yard result: %A" shuntingYardAstResult
    Assert.AreEqual (shuntingYardAstResult, expr)
//    printfn "passed Shunting Yard converstion to AST step"

    // Verify result of AST evaluator
    let evalResult = ArithmeticExpressionEvaluator.Semantic.eval shuntingYardAstResult
//    printfn "expected result: %A" result
//    printfn "function result: %A" evalResult
    Assert.AreEqual (evalResult, result)
//    printfn "passed AST eval step"

    // Verify result of Precedence Climbing
    let precedenceClimbingAstResult = ArithmeticExpressionEvaluator.PrecedenceClimbing.precedenceClimbing lexResult
//    printfn "expr: %A" expr
//    printfn "Precedence Climbing result: %A" precedenceClimbingAstResult
    Assert.AreEqual (precedenceClimbingAstResult, expr)
//    printfn "passed Precedence Climbing step"

    // Verify result of expression evaluator
    let evalResult = ArithmeticExpressionEvaluator.Semantic.eval precedenceClimbingAstResult
//    printfn "expected result: %A" result
//    printfn "function result: %A" evalResult
    Assert.AreEqual (evalResult, result)
//    printfn "passed eval step"
