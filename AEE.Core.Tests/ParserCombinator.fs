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

module ArithmeticExpressionEvaluator.ParserCombinator.Tests

//#region opens

open ArithmeticExpressionEvaluator.ParserCombinator
open ArithmeticExpressionEvaluator.PrefixLexer              // Needed for type token

open NUnit.Framework

// #endregion

//#region "helper functions"

// functions to help with test cases.

// Note: HOL Light parsers that work with parsing exceptions expect noparse exception
// and not a user defined exception.

let isTokenInteger x =
    match x with
    | Integer y ->
//        printfn "x: %A is token num." x
        true
    | _ ->
//        printfn "x: %A is NOT token num." x
        false

// Create a parser that accepts only decimal digit string values.
let digitStringParser (l : string list) : string * string list =
    match l with
    | h::t when ArithmeticExpressionEvaluator.PrefixLexer.isdecimaldigit h -> (h,t)
    | _ -> raise Noparse

// Create a parser that accepts only operator string values.
let symbolStringParser (l : string list) : string * string list =
    match l with
    | h::t when ArithmeticExpressionEvaluator.PrefixLexer.isoperator h -> (h,t)
    | _ -> raise Noparse

// Create a parser that accepts only Integer token values.
let intTokenParser (l : token list) : token * token list =
    match l with
    | Integer h::t when ArithmeticExpressionEvaluator.PrefixLexer.isdecimaldigit h -> (Integer h,t)
    | _ -> raise Noparse

// Create a parser that accepts only Operator token values.
let operatorTokenParser (l : token list) : token * token list =
    match l with
    | Operator h::t when ArithmeticExpressionEvaluator.PrefixLexer.isoperator h -> (Operator h,t)
    | _ -> raise Noparse

//#endregion

//#region "some tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private someStringTypeValues : (string * string list * (string * string list))[] = [|
    (
        // idx 0
        // ParserCombinator.some.01
        // No input
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.some function reads this
        ("",[]) // dummy value
    );
    (
        // idx 1
        // ParserCombinator.some.02
        // one char input, one value that matches
        "1",
        ["1"],
        ("1",[])
    );
    (
        // idx 2
        // ParserCombinator.some.03
        // one char input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a",
        ["a"],
        ("",[]) // dummy value
    );
    (
        // idx 3
        // ParserCombinator.some.04
        // two char input, two values that matches
        "12",
        ["1";"2"],
        ("1", ["2"])
    );
    (
        // idx 4
        // ParserCombinator.some.05
        // two char input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a1",
        ["a";"1"],
        ("",[]) // dummy value
    );
    (
        // idx 5
        // ParserCombinator.some.06
        // two char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "ab",
        ["a";"b"],
        ("",[]) // dummy value
    );
    (
        // idx 6
        // ParserCombinator.some.07
        // three char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "abc",
        ["a";"b";"c"],
        ("",[]) // dummy value
    );
    (
        // idx 7
        // ParserCombinator.some.08
        // three char input, first values matches
        "1bc",
        ["1";"b";"c"],
        ("1",["b";"c"])
    );
    (
        // idx 8
        // ParserCombinator.some.09
        // three char input, first two values match
        "12c",
        ["1";"2";"c"],
        ("1",["2";"c"])
    );
    (
        // idx 9
        // ParserCombinator.some.10
        // three char input, all values match
        "123",
        ["1";"2";"3"],
        ("1", ["2";"3"])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.some.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.some.02")>]
[<TestCase(2, TestName = "ParserCombinator.some.03", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.some.04")>]
[<TestCase(4, TestName = "ParserCombinator.some.05", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.some.06", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(6, TestName = "ParserCombinator.some.07", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(7, TestName = "ParserCombinator.some.08")>]
[<TestCase(8, TestName = "ParserCombinator.some.09")>]
[<TestCase(9, TestName = "ParserCombinator.some.010")>]
let ``function some - type string`` idx =
    let (externalForm, _, _) = someStringTypeValues.[idx]
    let (_, internalForm, _) = someStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = someStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> string * string list = ArithmeticExpressionEvaluator.ParserCombinator.some isdecimaldigit
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

// The first string is what humans expect to read
// and the second token list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private someTokenTypeValues : (string * token list * (token * token list))[] = [|
    (
        // idx 0
        // ParserCombinator.some.101
        // No input
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.some function reads this
        (Integer "0",[]) // dummy value
    );
    (
        // idx 1
        // ParserCombinator.some.102
        // one token input, one value that matches
        "1",
        [Integer "1"],
        (Integer "1",[])
    );
    (
        // idx 2
        // ParserCombinator.some.103
        // one token input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "(",
        [OpenParen],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 3
        // ParserCombinator.some.104
        // two token input, one value that matches
        "12(",
        [Integer "12"; OpenParen],
        (Integer "12",[OpenParen])
    );
    (
        // idx 4
        // ParserCombinator.some.105
        // two token input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( 1",
        [OpenParen; Integer "1"],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 5
        // ParserCombinator.some.106
        // two token input with space seperator, first value matches second value doesn't match
        "12 (",
        [Integer "12";OpenParen],
        (Integer "12",[OpenParen])
    );
    (
        // idx 6
        // ParserCombinator.some.107
        // two token input, first value matches second value doesn't match
        "12(",
        [Integer "12";OpenParen],
        (Integer "12",[OpenParen])
    );

    (
        // idx 7
        // ParserCombinator.some.108
        // three token input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( , ) ",
        [OpenParen; Comma; CloseParen],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 8
        // ParserCombinator.some.109
        // three token input, first values matches
        "1 ( )",
        [Integer "1"; OpenParen; CloseParen],
        (Integer "1",[OpenParen; CloseParen])
    );
    (
        // idx 9
        // ParserCombinator.some.110
        // three token input, first two values match
        "1 2 (",
        [Integer "1";Integer "2";OpenParen],
        (Integer "1", [Integer "2";OpenParen])
    );
    (
        // idx 10
        // ParserCombinator.some.111
        // three token input, all values match
        "1 2 3",
        [Integer "1";Integer "2";Integer "3"],
        (Integer "1", [Integer "2";Integer "3"])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.some.101", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.some.102")>]
[<TestCase(2, TestName = "ParserCombinator.some.103", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.some.104")>]
[<TestCase(4, TestName = "ParserCombinator.some.105", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.some.106")>]
[<TestCase(6, TestName = "ParserCombinator.some.107")>]
[<TestCase(7, TestName = "ParserCombinator.some.108", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(8, TestName = "ParserCombinator.some.109")>]
[<TestCase(9, TestName = "ParserCombinator.some.110")>]
[<TestCase(10, TestName = "ParserCombinator.some.111")>]
let ``function some - type token`` idx =
    let (externalForm, _, _) = someTokenTypeValues.[idx]
    let (_, internalForm, _) = someTokenTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = someTokenTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = (ArithmeticExpressionEvaluator.PrefixLexer.prefixLex << ArithmeticExpressionEvaluator.Lib.explode) externalForm  // Notice use of lex to convert string to token.
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let tokenParser : token list -> token * token list = ArithmeticExpressionEvaluator.ParserCombinator.some isTokenInteger
    let (current, rest) = tokenParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "a tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private aStringTypeValues : (string * string list * (string * string list))[] = [|
    (
        // idx 0
        // ParserCombinator.a.01
        // No input
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.a function reads this
        ("",[]) // dummy value
    );
    (
        // idx 1
        // ParserCombinator.a.02
        // one char input, one value that matches
        "1",
        ["1"],
        ("1",[])
    );
    (
        // idx 2
        // ParserCombinator.a.03
        // one char input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a",
        ["a"],
        ("",[]) // dummy value
    );
    (
        // idx 3
        // ParserCombinator.a.04
        // two char input, two values that matches
        "12",
        ["1";"2"],
        ("1", ["2"])
    );
    (
        // idx 4
        // ParserCombinator.a.05
        // two char input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a1",
        ["a";"1"],
        ("",[]) // dummy value
    );
    (
        // idx 5
        // ParserCombinator.a.06
        // two char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "ab",
        ["a";"b"],
        ("",[]) // dummy value
    );
    (
        // idx 6
        // ParserCombinator.a.07
        // three char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "abc",
        ["a";"b";"c"],
        ("",[]) // dummy value
    );
    (
        // idx 7
        // ParserCombinator.a.08
        // three char input, first values matches
        "1bc",
        ["1";"b";"c"],
        ("1",["b";"c"])
    );
    (
        // idx 8
        // ParserCombinator.a.09
        // three char input, first two values match
        "12c",
        ["1";"2";"c"],
        ("1",["2";"c"])
    );
    (
        // idx 9
        // ParserCombinator.a.10
        // three char input, all values match
        "123",
        ["1";"2";"3"],
        ("1", ["2";"3"])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.a.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.a.02")>]
[<TestCase(2, TestName = "ParserCombinator.a.03", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.a.04")>]
[<TestCase(4, TestName = "ParserCombinator.a.05", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.a.06", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(6, TestName = "ParserCombinator.a.07", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(7, TestName = "ParserCombinator.a.08")>]
[<TestCase(8, TestName = "ParserCombinator.a.09")>]
[<TestCase(9, TestName = "ParserCombinator.a.010")>]
let ``function a - type string`` idx =
    let (externalForm, _, _) = aStringTypeValues.[idx]
    let (_, internalForm, _) = aStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = aStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> string * string list = ArithmeticExpressionEvaluator.ParserCombinator.a "1"
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

// The first string is what humans expect to read
// and the second token list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private aTokenTypeValues : (string * token list * (token * token list))[] = [|
    (
        // idx 0
        // ParserCombinator.a.101
        // No input
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.a function reads this
        (Integer "0",[]) // dummy value
    );
    (
        // idx 1
        // ParserCombinator.a.102
        // one token input, one value that matches
        "1",
        [Integer "1"],
        (Integer "1",[])
    );
    (
        // idx 2
        // ParserCombinator.a.103
        // one token input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "(",
        [OpenParen],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 3
        // ParserCombinator.a.104
        // two token input, one value that matches
        "1+",
        [Integer "1"; Operator "+"],
        (Integer "1",[Operator "+"])
    );
    (
        // idx 4
        // ParserCombinator.a.105
        // two token input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( 1",
        [OpenParen; Integer "1"],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 5
        // ParserCombinator.a.106
        // two token input with space seperator, first value matches second value doesn't match
        "1 (",
        [Integer "1";OpenParen],
        (Integer "1",[OpenParen])
    );
    (
        // idx 6
        // ParserCombinator.a.107
        // two token input, first value matches second value doesn't match
        "1(",
        [Integer "1";OpenParen],
        (Integer "1",[OpenParen])
    );

    (
        // idx 7
        // ParserCombinator.a.108
        // three token input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( , ) ",
        [OpenParen; Comma; CloseParen],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 8
        // ParserCombinator.a.109
        // three token input, first values matches
        "1 ( )",
        [Integer "1"; OpenParen; CloseParen],
        (Integer "1",[OpenParen; CloseParen])
    );
    (
        // idx 9
        // ParserCombinator.a.110
        // three token input, first two values match
        "1 1 (",
        [Integer "1";Integer "1";OpenParen],
        (Integer "1", [Integer "1";OpenParen])
    );
    (
        // idx 10
        // ParserCombinator.a.111
        // three token input, all values match
        "1 1 1",
        [Integer "1";Integer "1";Integer "1"],
        (Integer "1", [Integer "1";Integer "1"])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.a.101", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.a.102")>]
[<TestCase(2, TestName = "ParserCombinator.a.103", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.a.104")>]
[<TestCase(4, TestName = "ParserCombinator.a.105", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.a.106")>]
[<TestCase(6, TestName = "ParserCombinator.a.107")>]
[<TestCase(7, TestName = "ParserCombinator.a.108", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(8, TestName = "ParserCombinator.a.109")>]
[<TestCase(9, TestName = "ParserCombinator.a.110")>]
[<TestCase(10, TestName = "ParserCombinator.a.111")>]
let ``function a - type token`` idx =
    let (externalForm, _, _) = aTokenTypeValues.[idx]
    let (_, internalForm, _) = aTokenTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = aTokenTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = (ArithmeticExpressionEvaluator.PrefixLexer.prefixLex << ArithmeticExpressionEvaluator.Lib.explode) externalForm  // Notice use of lex to convert string to token.
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let tokenParser : token list -> token * token list = ArithmeticExpressionEvaluator.ParserCombinator.a (Integer "1")
    let (current, rest) = tokenParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "alternative tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private alternativeStringTypeValues : (string * string list * (string * string list))[] = [|
    (
        // idx 0
        // ParserCombinator.alternative.01
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.alternative function reads this
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        ("",[]) // dummy value
    );
    (
        // idx 1
        // ParserCombinator.alternative.02
        // one char input, one value that matches
        "1",
        ["1"],
        ("1",[])
    );
    (
        // idx 2
        // ParserCombinator.alternative.03
        // one char input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a",
        ["a"],
        ("",[]) // dummy value
    );
    (
        // idx 3
        // ParserCombinator.alternative.04
        // two char input, two values that matches
        "1+",
        ["1";"+"],
        ("1",["+"])
    );
    (
        // idx 4
        // ParserCombinator.alternative.05
        // two char input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a1",
        ["a";"1"],
        ("",[]) // dummy value
    );
    (
        // idx 5
        // ParserCombinator.alternative.06
        // two char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "ab",
        ["a";"b"],
        ("",[]) // dummy value
    );
    (
        // idx 6
        // ParserCombinator.alternative.07
        // three char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "abc",
        ["a";"b";"c"],
        ("",[]) // dummy value
    );
    (
        // idx 7
        // ParserCombinator.alternative.08
        // three char input, first values matches
        "1bc",
        ["1";"b";"c"],
        ("1",["b";"c"])
    );
    (
        // idx 8
        // ParserCombinator.alternative.09
        // three char input, first two values match
        "12c",
        ["1";"2";"c"],
        ("1",["2";"c"])
    );
    (
        // idx 9
        // ParserCombinator.alternative.10
        // three char input, all values match
        "123",
        ["1";"2";"3"],
        ("1",["2";"3"])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.alternative.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.alternative.02")>]
[<TestCase(2, TestName = "ParserCombinator.alternative.03", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.alternative.04")>]
[<TestCase(4, TestName = "ParserCombinator.alternative.05", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.alternative.06", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(6, TestName = "ParserCombinator.alternative.07", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(7, TestName = "ParserCombinator.alternative.08")>]
[<TestCase(8, TestName = "ParserCombinator.alternative.09")>]
[<TestCase(9, TestName = "ParserCombinator.alternative.010")>]
let ``function alternative - type string`` idx =
    let (externalForm, _, _) = alternativeStringTypeValues.[idx]
    let (_, internalForm, _) = alternativeStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = alternativeStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> string * string list = ArithmeticExpressionEvaluator.ParserCombinator.op_LessBarGreater digitStringParser symbolStringParser  // (<|>)
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

// The first string is what humans expect to read
// and the second token list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private alternativeTokenTypeValues : (string * token list * (token * token list))[] = [|
    (
        // idx 0
        // ParserCombinator.alternative.101
        // No input
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.alternative function reads this
        (Integer "0",[]) // dummy value
    );
    (
        // idx 1
        // ParserCombinator.alternative.102
        // one token input, one value that matches
        "1",
        [Integer "1"],
        (Integer "1",[])
    );
    (
        // idx 2
        // ParserCombinator.alternative.103
        // one token input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "(",
        [OpenParen],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 3
        // ParserCombinator.alternative.104
        // two token input, one value that matches
        "1(",
        [Integer "1"; OpenParen],
        (Integer "1",[OpenParen])
    );
    (
        // idx 4
        // ParserCombinator.alternative.105
        // two token input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( 1",
        [OpenParen; Integer "1"],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 5
        // ParserCombinator.alternative.106
        // two token input with space seperator, first value matches second value doesn't match
        "12 (",
        [Integer "12";OpenParen],
        (Integer "12",[OpenParen])
    );
    (
        // idx 6
        // ParserCombinator.alternative.107
        // two token input, first value matches second value doesn't match
        "12(",
        [Integer "12";OpenParen],
        (Integer "12",[OpenParen])
    );
    (
        // idx 7
        // ParserCombinator.alternative.108
        // three token input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( , ) ",
        [OpenParen; Comma; CloseParen],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 8
        // ParserCombinator.alternative.109
        // three token input, first values matches
        "1 ( )",
        [Integer "1"; OpenParen; CloseParen],
        (Integer "1",[OpenParen; CloseParen])
    );
    (
        // idx 9
        // ParserCombinator.alternative.110
        // three token input, first two values match
        "1 2 (",
        [Integer "1";Integer "2";OpenParen],
        (Integer "1", [Integer "2"; OpenParen])
    );
    (
        // idx 10
        // ParserCombinator.alternative.111
        // three token input, all values match
        "1 2 3",
        [Integer "1";Integer "2";Integer "3"],
        (Integer "1", [Integer "2";Integer "3"])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.alternative.101", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.alternative.102")>]
[<TestCase(2, TestName = "ParserCombinator.alternative.103", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.alternative.104")>]
[<TestCase(4, TestName = "ParserCombinator.alternative.105", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.alternative.106")>]
[<TestCase(6, TestName = "ParserCombinator.alternative.107")>]
[<TestCase(7, TestName = "ParserCombinator.alternative.108", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(8, TestName = "ParserCombinator.alternative.109")>]
[<TestCase(9, TestName = "ParserCombinator.alternative.110")>]
[<TestCase(10, TestName = "ParserCombinator.alternative.111")>]
let ``function alternative - type token`` idx =
    let (externalForm, _, _) = alternativeTokenTypeValues.[idx]
    let (_, internalForm, _) = alternativeTokenTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = alternativeTokenTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = (ArithmeticExpressionEvaluator.PrefixLexer.prefixLex << ArithmeticExpressionEvaluator.Lib.explode) externalForm  // Notice use of lex to convert string to token.
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let tokenParser = ArithmeticExpressionEvaluator.ParserCombinator.op_LessBarGreater intTokenParser operatorTokenParser  // (<|>)
    let (current, rest) = tokenParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "sequential tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private sequentialStringTypeValues : (string * string list * ((string * string) * string list))[] = [|
    (
        // idx 0
        // ParserCombinator.sequential.01
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.sequential function reads this
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        (("",""),[]) // dummy value
    );
    (
        // idx 1
        // ParserCombinator.sequential.02
        // one char input
        // fails because need two sequential values to match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "1",
        ["1"],
        (("",""),[]) // dummy value
    );
    (
        // idx 2
        // ParserCombinator.sequential.03
        // one char input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a",
        ["a"],
        (("",""),[]) // dummy value
    );
    (
        // idx 3
        // ParserCombinator.sequential.04
        // two char input, two values that matches
        "1+",
        ["1";"+"],
        (("1", "+"), [])
    );
    (
        // idx 4
        // ParserCombinator.sequential.05
        // two char input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a+",
        ["a";"+"],
        (("",""),[]) // dummy value
    );
    (
        // idx 5
        // ParserCombinator.sequential.06
        // two char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "ab",
        ["a";"b"],
        (("",""),[]) // dummy value
    );
    (
        // idx 6
        // ParserCombinator.sequential.07
        // three char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "abc",
        ["a";"b";"c"],
        (("",""),[]) // dummy value
    );
    (
        // idx 7
        // ParserCombinator.sequential.08
        // three char input, first char matches, second char does not match
        "1bc",
        ["1";"b";"c"],
        (("",""),[]) // dummy value
    );
    (
        // idx 8
        // ParserCombinator.sequential.09
        // three char input, first two values match
        "1+c",
        ["1";"+";"c"],
        (("1", "+"), ["c"])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.sequential.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.sequential.02", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(2, TestName = "ParserCombinator.sequential.03", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.sequential.04")>]
[<TestCase(4, TestName = "ParserCombinator.sequential.05", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.sequential.06", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(6, TestName = "ParserCombinator.sequential.07", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(7, TestName = "ParserCombinator.sequential.08", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(8, TestName = "ParserCombinator.sequential.09")>]
let ``function sequential - type string`` idx =
    let (externalForm, _, _) = sequentialStringTypeValues.[idx]
    let (_, internalForm, _) = sequentialStringTypeValues.[idx]
    let (_, _, (currentResult, restResult)) = sequentialStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> (string * string) * string list = ArithmeticExpressionEvaluator.ParserCombinator.op_DotGreaterGreaterDot digitStringParser symbolStringParser  // (.>>.)
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

// The first string is what humans expect to read
// and the second token list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private sequentialTokenTypeValues : (string * token list * ((token * token) * token list))[] = [|
    (
        // idx 0
        // ParserCombinator.sequential.101
        // No input
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.sequential function reads this
        ((Integer "0", Integer "0"),[]) // dummy value
    );
    (
        // idx 1
        // ParserCombinator.sequential.102
        // one token input
        // fails because need two sequential values to match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "1",
        [Integer "1"],
        ((Integer "0", Integer "0"),[]) // dummy value
    );
    (
        // idx 2
        // ParserCombinator.sequential.103
        // one token input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "(",
        [OpenParen],
        ((Integer "0", Integer "0"),[]) // dummy value
    );
    (
        // idx 3
        // ParserCombinator.sequential.104
        // two token input, one value that matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "1(",
        [Integer "1"; OpenParen],
        ((Integer "0", Integer "0"),[]) // dummy value
    );
    (
        // idx 4
        // ParserCombinator.sequential.105
        // two token input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( +",
        [OpenParen; Operator "+"],
        ((Integer "0", Integer "0"),[]) // dummy value
    );
    (
        // idx 5
        // ParserCombinator.sequential.106
        // two token input with space seperator, first value matches second value doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "12 (",
        [Integer "12";OpenParen],
        ((Integer "0", Integer "0"),[]) // dummy value
    );
    (
        // idx 6
        // ParserCombinator.sequential.107
        // two token input, first value matches second value matches
        "12+",
        [Integer "12";Operator "+"],
        ((Integer "12", Operator "+"), [])
    );
    (
        // idx 7
        // ParserCombinator.sequential.108
        // three token input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( , ) ",
        [OpenParen; Comma; CloseParen],
        ((Integer "0", Integer "0"),[]) // dummy value
    );
    (
        // idx 8
        // ParserCombinator.sequential.109
        // three token input, first values matches
        "1 ( )",
        [Integer "1"; OpenParen; CloseParen],
        ((Integer "0", Integer "0"),[]) // dummy value
    );
    (
        // idx 9
        // ParserCombinator.sequential.110
        // three token input, first two values match
        "12 + 1",
        [Integer "12";Operator "+";Integer "1"],
        ((Integer "12", Operator "+"), [Integer "1"])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.sequential.101", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.sequential.102", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(2, TestName = "ParserCombinator.sequential.103", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.sequential.104", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(4, TestName = "ParserCombinator.sequential.105", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.sequential.106", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(6, TestName = "ParserCombinator.sequential.107")>]
[<TestCase(7, TestName = "ParserCombinator.sequential.108", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(8, TestName = "ParserCombinator.sequential.109", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(9, TestName = "ParserCombinator.sequential.110")>]
let ``function sequential - type token`` idx =
    let (externalForm, _, _) = sequentialTokenTypeValues.[idx]
    let (_, internalForm, _) = sequentialTokenTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = sequentialTokenTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = (ArithmeticExpressionEvaluator.PrefixLexer.prefixLex << ArithmeticExpressionEvaluator.Lib.explode) externalForm  // Notice use of lex to convert string to token.
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let tokenParser = ArithmeticExpressionEvaluator.ParserCombinator.op_DotGreaterGreaterDot intTokenParser operatorTokenParser  // (.>>.)
    let (current, rest) = tokenParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "many tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private manyStringTypeValues : (string * string list * (string list * string list))[] = [|
    (
        // idx 0
        // ParserCombinator.many.01
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.many function reads this
        ([],[])
    );
    (
        // idx 1
        // ParserCombinator.many.02
        // one char input, one value that matches
        "1",
        ["1"],
        (["1"],[])
    );
    (
        // idx 2
        // ParserCombinator.many.03
        // one char input, one value that doesn't match
        "a",
        ["a"],
        ([],["a"])
    );
    (
        // idx 3
        // ParserCombinator.many.04
        // two char input, two values that matches
        "12",
        ["1";"2"],
        (["1"; "2"],[])
    );
    (
        // idx 4
        // ParserCombinator.many.05
        // two char input, first value doesn't match, second value matches
        "a1",
        ["a";"1"],
        ([],["a";"1"])
    );
    (
        // idx 5
        // ParserCombinator.many.06
        // two char input, no values match
        "ab",
        ["a";"b"],
        ([],["a";"b"])
    );
    (
        // idx 6
        // ParserCombinator.many.07
        // three char input, no values match
        "abc",
        ["a";"b";"c"],
        ([],["a";"b";"c"])
    );
    (
        // idx 7
        // ParserCombinator.many.08
        // three char input, first values matches
        "1bc",
        ["1";"b";"c"],
        (["1"],["b";"c"])
    );
    (
        // idx 8
        // ParserCombinator.many.09
        // three char input, first two values match
        "12c",
        ["1";"2";"c"],
        (["1";"2"],["c"])
    );
    (
        // idx 9
        // ParserCombinator.many.10
        // three char input, all values match
        "123",
        ["1";"2";"3"],
        (["1";"2";"3"],[])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.many.01")>]
[<TestCase(1, TestName = "ParserCombinator.many.02")>]
[<TestCase(2, TestName = "ParserCombinator.many.03")>]
[<TestCase(3, TestName = "ParserCombinator.many.04")>]
[<TestCase(4, TestName = "ParserCombinator.many.05")>]
[<TestCase(5, TestName = "ParserCombinator.many.06")>]
[<TestCase(6, TestName = "ParserCombinator.many.07")>]
[<TestCase(7, TestName = "ParserCombinator.many.08")>]
[<TestCase(8, TestName = "ParserCombinator.many.09")>]
[<TestCase(9, TestName = "ParserCombinator.many.010")>]
let ``function many - type string`` idx =
    let (externalForm, _, _) = manyStringTypeValues.[idx]
    let (_, internalForm, _) = manyStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = manyStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> string list * string list = ArithmeticExpressionEvaluator.ParserCombinator.many digitStringParser
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

// The first string is what humans expect to read
// and the second token list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private manyTokenTypeValues : (string * token list * (token list * token list))[] = [|
    (
        // idx 0
        // ParserCombinator.many.101
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.many function reads this
        ([],[])
    );
    (
        // idx 1
        // ParserCombinator.many.102
        // one token input, one value that matches
        "1",
        [Integer "1"],
        ([Integer "1"],[])
    );
    (
        // idx 2
        // ParserCombinator.many.103
        // one token input, one value that doesn't match
        "(",
        [OpenParen],
        ([],[OpenParen])
    );
    (
        // idx 3
        // ParserCombinator.many.104
        // two token input, one value that matches
        "12",
        [Integer "12"],
        ([Integer "12"],[])
    );
    (
        // idx 4
        // ParserCombinator.many.105
        // two token input, first value doesn't match, second value matches
        "( 1",
        [OpenParen; Integer "1"],
        ([],[OpenParen; Integer "1"])
    );
    (
        // idx 5
        // ParserCombinator.many.106
        // two token input with space seperator, first value matches second value doesn't match
        "12 (",
        [Integer "12";OpenParen],
        ([Integer "12"],[OpenParen])
    );
    (
        // idx 6
        // ParserCombinator.many.107
        // two token input, first value matches second value doesn't match
        "12(",
        [Integer "12";OpenParen],
        ([Integer "12"],[OpenParen])
    );

    (
        // idx 7
        // ParserCombinator.many.108
        // three token input, no values match
        "( , ) ",
        [OpenParen; Comma; CloseParen],
        ([],[OpenParen; Comma; CloseParen])
    );
    (
        // idx 8
        // ParserCombinator.many.109
        // three token input, first values matches
        "1 ( )",
        [Integer "1"; OpenParen; CloseParen],
        ([Integer "1"],[OpenParen; CloseParen])
    );
    (
        // idx 9
        // ParserCombinator.many.110
        // three token input, first two values match
        "1 2 (",
        [Integer "1";Integer "2";OpenParen],
        ([Integer "1";Integer "2"],[OpenParen])
    );
    (
        // idx 10
        // ParserCombinator.many.111
        // three token input, all values match
        "1 2 3",
        [Integer "1";Integer "2";Integer "3"],
        ([Integer "1";Integer "2";Integer "3"],[])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.many.101")>]
[<TestCase(1, TestName = "ParserCombinator.many.102")>]
[<TestCase(2, TestName = "ParserCombinator.many.103")>]
[<TestCase(3, TestName = "ParserCombinator.many.104")>]
[<TestCase(4, TestName = "ParserCombinator.many.105")>]
[<TestCase(5, TestName = "ParserCombinator.many.106")>]
[<TestCase(6, TestName = "ParserCombinator.many.107")>]
[<TestCase(7, TestName = "ParserCombinator.many.108")>]
[<TestCase(8, TestName = "ParserCombinator.many.109")>]
[<TestCase(9, TestName = "ParserCombinator.many.110")>]
[<TestCase(10, TestName = "ParserCombinator.many.111")>]
let ``function many - type token`` idx =
    let (externalForm, _, _) = manyTokenTypeValues.[idx]
    let (_, internalForm, _) = manyTokenTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = manyTokenTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = (ArithmeticExpressionEvaluator.PrefixLexer.prefixLex << ArithmeticExpressionEvaluator.Lib.explode) externalForm  // Notice use of lex to convert string to token.
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let tokenParser : token list -> token list * token list = ArithmeticExpressionEvaluator.ParserCombinator.many intTokenParser
    let (current, rest) = tokenParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "atleast tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private atleastStringTypeValues : (string * string list * (string list * string list))[] = [|
    (
        // idx 0
        // ParserCombinator.atleast.01
        // No input
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.atleast function reads this
        ([],[])  // dummy value
    );
    (
        // idx 1
        // ParserCombinator.atleast.02
        // one char input, one value that matches
        "1",
        ["1"],
        (["1"],[])
    );
    (
        // idx 2
        // ParserCombinator.atleast.03
        // one char input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a",
        ["a"],
        ([],[])  // dummy value
    );
    (
        // idx 3
        // ParserCombinator.atleast.04
        // two char input, two values that matches
        "12",
        ["1";"2"],
        (["1"; "2"],[])
    );
    (
        // idx 4
        // ParserCombinator.atleast.05
        // two char input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a1",
        ["a";"1"],
        ([],[])  // dummy value
    );
    (
        // idx 5
        // ParserCombinator.atleast.06
        // two char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "ab",
        ["a";"b"],
        ([],[])  // dummy value
    );
    (
        // idx 6
        // ParserCombinator.atleast.07
        // three char input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "abc",
        ["a";"b";"c"],
        ([],[])  // dummy value
    );
    (
        // idx 7
        // ParserCombinator.atleast.08
        // three char input, first values matches
        "1bc",
        ["1";"b";"c"],
        (["1"],["b";"c"])
    );
    (
        // idx 8
        // ParserCombinator.atleast.09
        // three char input, first two values match
        "12c",
        ["1";"2";"c"],
        (["1";"2"],["c"])
    );
    (
        // idx 9
        // ParserCombinator.atleast.10
        // three char input, all values match
        "123",
        ["1";"2";"3"],
        (["1";"2";"3"],[])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.atleast.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.atleast.02")>]
[<TestCase(2, TestName = "ParserCombinator.atleast.03", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.atleast.04")>]
[<TestCase(4, TestName = "ParserCombinator.atleast.05", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.atleast.06", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(6, TestName = "ParserCombinator.atleast.07", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(7, TestName = "ParserCombinator.atleast.08")>]
[<TestCase(8, TestName = "ParserCombinator.atleast.09")>]
[<TestCase(9, TestName = "ParserCombinator.atleast.010")>]
let ``function atleast - type string`` idx =
    let (externalForm, _, _) = atleastStringTypeValues.[idx]
    let (_, internalForm, _) = atleastStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = atleastStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> string list * string list = ArithmeticExpressionEvaluator.ParserCombinator.atleast 1 digitStringParser
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

// The first string is what humans expect to read
// and the second token list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private atleastTokenTypeValues : (string * token list * (token list * token list))[] = [|
    (
        // idx 0
        // ParserCombinator.atleast.101
        // No input
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.atleast function reads this
        ([],[]) // dummy value
    );
    (
        // idx 1
        // ParserCombinator.atleast.102
        // one token input, one value that matches
        "1",
        [Integer "1"],
        ([Integer "1"],[])
    );
    (
        // idx 2
        // ParserCombinator.atleast.103
        // one token input, one value that doesn't match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "(",
        [OpenParen],
        ([],[]) // dummy value
    );
    (
        // idx 3
        // ParserCombinator.atleast.104
        // two token input, one value that matches
        "12",
        [Integer "12"],
        ([Integer "12"],[])
    );
    (
        // idx 4
        // ParserCombinator.atleast.105
        // two token input, first value doesn't match, second value matches
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( 1",
        [OpenParen; Integer "1"],
        ([],[]) // dummy value
    );
    (
        // idx 5
        // ParserCombinator.atleast.106
        // two token input with space seperator, first value matches second value doesn't match
        "12 (",
        [Integer "12";OpenParen],
        ([Integer "12"],[OpenParen])
    );
    (
        // idx 6
        // ParserCombinator.atleast.107
        // two token input, first value matches second value doesn't match
        "12(",
        [Integer "12";OpenParen],
        ([Integer "12"],[OpenParen])
    );

    (
        // idx 7
        // ParserCombinator.atleast.108
        // three token input, no values match
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "( , ) ",
        [OpenParen; Comma; CloseParen],
        ([],[]) // dummy value
    );
    (
        // idx 8
        // ParserCombinator.atleast.109
        // three token input, first values matches
        "1 ( )",
        [Integer "1"; OpenParen; CloseParen],
        ([Integer "1"],[OpenParen; CloseParen])
    );
    (
        // idx 9
        // ParserCombinator.atleast.110
        // three token input, first two values match
        "1 2 (",
        [Integer "1";Integer "2";OpenParen],
        ([Integer "1";Integer "2"],[OpenParen])
    );
    (
        // idx 10
        // ParserCombinator.atleast.111
        // three token input, all values match
        "1 2 3",
        [Integer "1";Integer "2";Integer "3"],
        ([Integer "1";Integer "2";Integer "3"],[])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.atleast.101", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "ParserCombinator.atleast.102")>]
[<TestCase(2, TestName = "ParserCombinator.atleast.103", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(3, TestName = "ParserCombinator.atleast.104")>]
[<TestCase(4, TestName = "ParserCombinator.atleast.105", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "ParserCombinator.atleast.106")>]
[<TestCase(6, TestName = "ParserCombinator.atleast.107")>]
[<TestCase(7, TestName = "ParserCombinator.atleast.108", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(8, TestName = "ParserCombinator.atleast.109")>]
[<TestCase(9, TestName = "ParserCombinator.atleast.110")>]
[<TestCase(10, TestName = "ParserCombinator.atleast.111")>]
let ``function atleast - type token`` idx =
    let (externalForm, _, _) = atleastTokenTypeValues.[idx]
    let (_, internalForm, _) = atleastTokenTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = atleastTokenTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = (ArithmeticExpressionEvaluator.PrefixLexer.prefixLex << ArithmeticExpressionEvaluator.Lib.explode) externalForm  // Notice use of lex to convert string to token.
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let tokenParser : token list -> token list * token list = ArithmeticExpressionEvaluator.ParserCombinator.atleast 1 intTokenParser
    let (current, rest) = tokenParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "finished tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private finishedStringTypeValues : (string * string list * (int * string list))[] = [|
    (
        // idx 0
        // ParserCombinator.finished.01
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.finished function reads this
        (0,[])
    );
    (
        // idx 1
        // ParserCombinator.finished.02
        // one char input, one value that matches
        // throws System.Exception "Unparsed input"
        "1",
        ["1"],
        (0,[])  // dummy value
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.finished.01")>]
[<TestCase(1, TestName = "ParserCombinator.finished.02", ExpectedException=typeof<System.Exception>, ExpectedMessage = "Unparsed input")>]
let ``function finished - type string`` idx =
    let (externalForm, _, _) = finishedStringTypeValues.[idx]
    let (_, internalForm, _) = finishedStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = finishedStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> int * string list = ArithmeticExpressionEvaluator.ParserCombinator.finished
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

// The first string is what humans expect to read
// and the second token list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private finishedTokenTypeValues : (string * token list * (int * token list))[] = [|
    (
        // idx 0
        // ParserCombinator.finished.101
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.ParserCombinator.finished function reads this
        (0,[])
    );
    (
        // idx 1
        // ParserCombinator.finished.102
        // one token input, one value that matches
        // throws System.Exception "Unparsed input"
        "1",
        [Integer "1"],
        (0,[]) // dummy value
    );
    |]

[<Test>]
[<TestCase(0, TestName = "ParserCombinator.finished.101")>]
[<TestCase(1, TestName = "ParserCombinator.finished.102", ExpectedException=typeof<System.Exception>, ExpectedMessage = "Unparsed input")>]

let ``function finished - type token`` idx =
    let (externalForm, _, _) = finishedTokenTypeValues.[idx]
    let (_, internalForm, _) = finishedTokenTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = finishedTokenTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = (ArithmeticExpressionEvaluator.PrefixLexer.prefixLex << ArithmeticExpressionEvaluator.Lib.explode) externalForm  // Notice use of lex to convert string to token.
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let tokenParser : token list -> int * token list = ArithmeticExpressionEvaluator.ParserCombinator.finished
    let (current, rest) = tokenParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "apply function (|>>) tests"

// Test using |>> List.reduceBack (+)
// to convert (string list) to string

[<Test>]
let ``ParserCombinator.applyfunction.01``() =
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode "12345"  
//    let parser (sl : string list) : string list * string list = atleast 1 (some isdecimaldigit) sl
//    let treatment (inp : string list) : string = List.reduceBack (+) inp
//    let parser' (sl : string list) : string * string list = (parser |>> treatment) sl
    let parser = atleast 1 (some isdecimaldigit)
    let treatment = List.reduceBack (+)
    let parser' = ArithmeticExpressionEvaluator.ParserCombinator.op_BarGreaterGreater parser treatment
    let result = parser' convertedForm
//    printfn "result: %A" result
    Assert.AreEqual(result, ("12345",([] : string list)))
    
// Test using |>> (fun x -> Integer x)
// to convert string to token

[<Test>]
let ``ParserCombinator.applyfunction.02``() =
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode "12345"  
//    let parser (sl : string list) : string * string list = stringof (some isdecimaldigit) sl
//    let treatment (inp : string) : token = (fun x -> Integer x) inp
//    let parser' (s : string list) : token * string list = (parser |>> treatment) s
    let parser  = stringof (some isdecimaldigit) 
    let treatment = (fun x -> Integer x)
    let parser' = ArithmeticExpressionEvaluator.ParserCombinator.op_BarGreaterGreater parser treatment
    let result = parser' convertedForm
//    printfn "result: %A" result
    Assert.AreEqual(result, (Integer "12345",([] : string list)))

// Test using |>> snd
// to convert (string list * token) to token

[<Test>]
let ``ParserCombinator.applyfunction.03``() =
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode "  12345"  
//    let parser (sl : string list) : (string list * token) * string list  = (many (some iswhitespace) .>>. rawtoken) sl
//    let treatment (inp : string list * token) : token = snd inp
//    let parser' (sl : string list) : token * string list = (parser |>> treatment) sl
    let parser = many (some iswhitespace) .>>. rawtoken
    let treatment = snd
    let parser' = ArithmeticExpressionEvaluator.ParserCombinator.op_BarGreaterGreater parser treatment
    let result = parser' convertedForm
//    printfn "result: %A" result
    Assert.AreEqual(result, (Integer "12345",([] : string list)))

// Test using |>> (fst << fst)
// to convert ((token list * string list) * int) to token list

[<Test>]
let ``ParserCombinator.applyfunction.04``() =
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode "  12345   "
//    let parser (sl : string list) : ((token list * string list) * int) * string list = (tokens .>>. many(some iswhitespace) .>>. finished) sl
//    let treatment (inp : ((token list * string list) * int)) : token list = (fst << fst) inp
//    let parser' (sl : string list) : token list * string list = (parser |>> treatment) sl
    let parser = (tokens .>>. many(some iswhitespace) .>>. finished)
    let treatment = (fst << fst)
    let parser' = ArithmeticExpressionEvaluator.ParserCombinator.op_BarGreaterGreater parser treatment
    let result = parser' convertedForm
//    printfn "result: %A" result
    Assert.AreEqual(result, ([Integer "12345"],([] : string list)))

//#endregion