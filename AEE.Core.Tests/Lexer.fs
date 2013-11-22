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

module ArithmeticExpressionEvaluator.Lexer.Tests

open ArithmeticExpressionEvaluator.Lexer

open NUnit.Framework

//#region "iswhitespace tests"

let private iswhitespaceValues : (string * bool)[] = [|
    (
        // idx 0
        // Lexer.iswhitespace.01
        // No input
        // System.IndexOutOfRangeException - "Index was outside the bounds of the array."
        // The error is because the function uses an array and there is no char to covert to an index into the array.
        "",
        false
    );
    (
        // idx 1
        // Lexer.iswhitespace.02
        // one space
        " ",
        true
    );
    (
        // idx 2
        // Lexer.iswhitespace.03
        // one tab
        "\t",
        true
    );
    (
        // idx 3
        // Lexer.iswhitespace.04
        // one newline
        "\n",
        true
    );
    (
        // idx 4
        // Lexer.iswhitespace.05
        // one carriage return
        "\r",
        true
    );
    (
        // idx 5
        // Lexer.iswhitespace.06
        // one space then one letter
        " a",
        true
    );
    (
        // idx 6
        // Lexer.iswhitespace.07
        // one tab then one letter
        "\ta",
        true
    );
    (
        // idx 7
        // Lexer.iswhitespace.08
        // one newline then one letter
        "\na",
        true
    );
    (
        // idx 8
        // Lexer.iswhitespace.09
        // one carriage return then one letter
        "\ra",
        true
    );
    (
        // idx 9
        // Lexer.iswhitespace.10
        // one letter
        "a",
        false
    );
    (
        // idx 10
        // Lexer.iswhitespace.11
        // one decimal digit
        "1",
        false
    );
    (
        // idx 11
        // Lexer.iswhitespace.12
        // one letter and one space
        "a ",
        false
    );
    |]
[<Test>]
[<TestCase(0, TestName = "Lexer.iswhitespace.01", ExpectedException=typeof<System.IndexOutOfRangeException>, ExpectedMessage = "Index was outside the bounds of the array.")>]
[<TestCase(1, TestName = "Lexer.iswhitespace.02")>]
[<TestCase(2, TestName = "Lexer.iswhitespace.03")>]
[<TestCase(3, TestName = "Lexer.iswhitespace.04")>]
[<TestCase(4, TestName = "Lexer.iswhitespace.05")>]
[<TestCase(5, TestName = "Lexer.iswhitespace.06")>]
[<TestCase(6, TestName = "Lexer.iswhitespace.07")>]
[<TestCase(7, TestName = "Lexer.iswhitespace.08")>]
[<TestCase(8, TestName = "Lexer.iswhitespace.09")>]
[<TestCase(9, TestName = "Lexer.iswhitespace.010")>]
[<TestCase(10, TestName = "Lexer.iswhitespace.011")>]
[<TestCase(11, TestName = "Lexer.iswhitespace.012")>]
let ``function lex.iswhitespace`` idx =
    let (input, _) = iswhitespaceValues.[idx]
    let (_, result) = iswhitespaceValues.[idx]

    let functionResult = ArithmeticExpressionEvaluator.Lexer.iswhitespace input
//    printfn "expected result: %A" result
//    printfn "function result: %A" functionResult
    Assert.AreEqual (result, functionResult)

//#endregion

//#region "isparen tests"

let private isparenValues : (string * bool)[] = [|
    (
        // idx 0
        // Lexer.isparen.01
        // No input
        // System.IndexOutOfRangeException - "Index was outside the bounds of the array."
        // The error is because the function uses an array and there is no char to covert to an index into the array.
        "",
        false
    );
    (
        // idx 1
        // Lexer.isparen.02
        // one open paren
        "(",
        true
    );
    (
        // idx 2
        // Lexer.isparen.03
        // one close paren
        ")",
        true
    );
    (
        // idx 3
        // Lexer.isparen.04
        // one open paren then one letter
        "(a",
        true
    );
    (
        // idx 4
        // Lexer.isparen.05
        // one close paren then one letter
        ")a",
        true
    );
    (
        // idx 5
        // Lexer.isparen.06
        // one letter
        "a",
        false
    );
    (
        // idx 6
        // Lexer.isparen.07
        // one decimal digit
        "1",
        false
    );
    (
        // idx 7
        // Lexer.isparen.08
        // one letter and one open paren
        "a(",
        false
    );
    |]
[<Test>]
[<TestCase(0, TestName = "Lexer.isparen.01", ExpectedException=typeof<System.IndexOutOfRangeException>, ExpectedMessage = "Index was outside the bounds of the array.")>]
[<TestCase(1, TestName = "Lexer.isparen.02")>]
[<TestCase(2, TestName = "Lexer.isparen.03")>]
[<TestCase(3, TestName = "Lexer.isparen.04")>]
[<TestCase(4, TestName = "Lexer.isparen.05")>]
[<TestCase(5, TestName = "Lexer.isparen.06")>]
[<TestCase(6, TestName = "Lexer.isparen.07")>]
[<TestCase(7, TestName = "Lexer.isparen.08")>]
let ``function lex.isparen`` idx =
    let (input, _) = isparenValues.[idx]
    let (_, result) = isparenValues.[idx]

    let functionResult = ArithmeticExpressionEvaluator.Lexer.isparen input
//    printfn "expected result: %A" result
//    printfn "function result: %A" functionResult
    Assert.AreEqual (result, functionResult)

//#endregion

//#region "isoperator tests"

let private isoperatorValues : (string * bool)[] = [|
    (
        // idx 0
        // Lexer.isoperator.01
        // No input
        // System.IndexOutOfRangeException - "Index was outside the bounds of the array."
        // The error is because the function uses an array and there is no char to covert to an index into the array.
        "",
        false
    );
    (
        // idx 1
        // Lexer.isoperator.02
        // one star
        "*",
        true
    );
    (
        // idx 2
        // Lexer.isoperator.03
        // one plus
        "+",
        true
    );
    (
        // idx 3
        // Lexer.isoperator.04
        // one dash
        "-",
        true
    );
    (
        // idx 4
        // Lexer.isoperator.05
        // one slash
        "/",
        true
    );
    (
        // idx 5
        // Lexer.isoperator.06
        // one star then one letter
        "*a",
        true
    );
    (
        // idx 6
        // Lexer.isoperator.07
        // one plus then one letter
        "+a",
        true
    );
    (
        // idx 7
        // Lexer.isoperator.08
        // one dash then one letter
        "-a",
        true
    );
    (
        // idx 8
        // Lexer.isoperator.09
        // one slash then one letter
        "/a",
        true
    );
    (
        // idx 9
        // Lexer.isoperator.10
        // one letter
        "a",
        false
    );
    (
        // idx 10
        // Lexer.isoperator.11
        // one decimal digit
        "1",
        false
    );
    (
        // idx 11
        // Lexer.isoperator.12
        // one letter and one star
        "a*",
        false
    );
    |]
[<Test>]
[<TestCase(0, TestName = "Lexer.isoperator.01", ExpectedException=typeof<System.IndexOutOfRangeException>, ExpectedMessage = "Index was outside the bounds of the array.")>]
[<TestCase(1, TestName = "Lexer.isoperator.02")>]
[<TestCase(2, TestName = "Lexer.isoperator.03")>]
[<TestCase(3, TestName = "Lexer.isoperator.04")>]
[<TestCase(4, TestName = "Lexer.isoperator.05")>]
[<TestCase(5, TestName = "Lexer.isoperator.06")>]
[<TestCase(6, TestName = "Lexer.isoperator.07")>]
[<TestCase(7, TestName = "Lexer.isoperator.08")>]
[<TestCase(8, TestName = "Lexer.isoperator.09")>]
[<TestCase(9, TestName = "Lexer.isoperator.010")>]
[<TestCase(10, TestName = "Lexer.isoperator.011")>]
[<TestCase(11, TestName = "Lexer.isoperator.012")>]
let ``function lex.isoperator`` idx =
    let (input, _) = isoperatorValues.[idx]
    let (_, result) = isoperatorValues.[idx]

    let functionResult = ArithmeticExpressionEvaluator.Lexer.isoperator input
//    printfn "expected result: %A" result
//    printfn "function result: %A" functionResult
    Assert.AreEqual (result, functionResult)

//#endregion

//#region "isdecimaldigit tests"

let private isdecimaldigitValues : (string * bool)[] = [|
    (
        // idx 0
        // Lexer.isdecimaldigit.001
        // No input
        // System.IndexOutOfRangeException - "Index was outside the bounds of the array."
        // The error is because the function uses an array and there is no char to covert to an index into the array.
        "",
        false
    );
    (
        // idx 1
        // Lexer.isdecimaldigit.002
        // one one character
        "1",
        true
    );
    (
        // idx 2
        // Lexer.isdecimaldigit.003
        // one two character
        "2",
        true
    );
    (
        // idx 3
        // Lexer.isdecimaldigit.004
        // one three character
        "3",
        true
    );
    (
        // idx 4
        // Lexer.isdecimaldigit.005
        // one four character
        "4",
        true
    );
    (
        // idx 5
        // Lexer.isdecimaldigit.006
        // one five character
        "5",
        true
    );
    (
        // idx 6
        // Lexer.isdecimaldigit.007
        // one six character
        "6",
        true
    );
    (
        // idx 7
        // Lexer.isdecimaldigit.008
        // one seven character
        "7",
        true
    );
    (
        // idx 8
        // Lexer.isdecimaldigit.009
        // one eight character
        "8",
        true
    );
    (
        // idx 9
        // Lexer.isdecimaldigit.010
        // one nine character
        "9",
        true
    );
    (
        // idx 10
        // Lexer.isdecimaldigit.011
        // one zero character
        "0",
        true
    );
    (
        // idx 11
        // Lexer.isdecimaldigit.012
        // one one character then one letter
        "1a",
        true
    );
    (
        // idx 12
        // Lexer.isdecimaldigit.013
        // one two character then one letter
        "2a",
        true
    );
    (
        // idx 13
        // Lexer.isdecimaldigit.014
        // one three character then one letter
        "3a",
        true
    );
    (
        // idx 14
        // Lexer.isdecimaldigit.015
        // one four character then one letter
        "4a",
        true
    );
    (
        // idx 15
        // Lexer.isdecimaldigit.016
        // one five character then one letter
        "5a",
        true
    );
    (
        // idx 16
        // Lexer.isdecimaldigit.017
        // one six character then one letter
        "6a",
        true
    );
    (
        // idx 17
        // Lexer.isdecimaldigit.018
        // one seven character then one letter
        "7a",
        true
    );
    (
        // idx 18
        // Lexer.isdecimaldigit.019
        // one eight character then one letter
        "8a",
        true
    );
    (
        // idx 19
        // Lexer.isdecimaldigit.020
        // one nine character then one letter
        "9a",
        true
    );
    (
        // idx 20
        // Lexer.isdecimaldigit.021
        // one zero character then one letter
        "0a",
        true
    );
    (
        // idx 21
        // Lexer.isdecimaldigit.22
        // one letter
        "a",
        false
    );
    (
        // idx 22
        // Lexer.isdecimaldigit.23
        // one star
        "*",
        false
    );
    (
        // idx 23
        // Lexer.isdecimaldigit.24
        // one letter and one one character
        "a1",
        false
    );
    |]
[<Test>]
[<TestCase(0, TestName = "Lexer.isdecimaldigit.01", ExpectedException=typeof<System.IndexOutOfRangeException>, ExpectedMessage = "Index was outside the bounds of the array.")>]
[<TestCase(1, TestName = "Lexer.isdecimaldigit.02")>]
[<TestCase(2, TestName = "Lexer.isdecimaldigit.03")>]
[<TestCase(3, TestName = "Lexer.isdecimaldigit.04")>]
[<TestCase(4, TestName = "Lexer.isdecimaldigit.05")>]
[<TestCase(5, TestName = "Lexer.isdecimaldigit.06")>]
[<TestCase(6, TestName = "Lexer.isdecimaldigit.07")>]
[<TestCase(7, TestName = "Lexer.isdecimaldigit.08")>]
[<TestCase(8, TestName = "Lexer.isdecimaldigit.09")>]
[<TestCase(9, TestName = "Lexer.isdecimaldigit.010")>]
[<TestCase(10, TestName = "Lexer.isdecimaldigit.011")>]
[<TestCase(11, TestName = "Lexer.isdecimaldigit.012")>]
[<TestCase(12, TestName = "Lexer.isdecimaldigit.013")>]
[<TestCase(13, TestName = "Lexer.isdecimaldigit.014")>]
[<TestCase(14, TestName = "Lexer.isdecimaldigit.015")>]
[<TestCase(15, TestName = "Lexer.isdecimaldigit.016")>]
[<TestCase(16, TestName = "Lexer.isdecimaldigit.017")>]
[<TestCase(17, TestName = "Lexer.isdecimaldigit.018")>]
[<TestCase(18, TestName = "Lexer.isdecimaldigit.019")>]
[<TestCase(19, TestName = "Lexer.isdecimaldigit.020")>]
[<TestCase(20, TestName = "Lexer.isdecimaldigit.021")>]
[<TestCase(21, TestName = "Lexer.isdecimaldigit.022")>]
[<TestCase(22, TestName = "Lexer.isdecimaldigit.023")>]
[<TestCase(23, TestName = "Lexer.isdecimaldigit.024")>]
let ``function lex.isdecimaldigit`` idx =
    let (input, _) = isdecimaldigitValues.[idx]
    let (_, result) = isdecimaldigitValues.[idx]

    let functionResult = ArithmeticExpressionEvaluator.Lexer.isdecimaldigit input
//    printfn "expected result: %A" result
//    printfn "function result: %A" functionResult
    Assert.AreEqual (result, functionResult)

//#endregion



//#region "stringof tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private stringofValues : (string * string list * (string -> bool) * (string * string list))[] = [|
    (
        // idx 0
        // Lexer.stringof.01
        // No input
        // throws noparse
        "",    // humans read this
        [],    // the stringof function reads this
        isdecimaldigit,
        ("",[])  // dummy value
    );
    (
        // idx 1
        // Lexer.stringof.02
        // one number
        "1",
        ["1"],
        isdecimaldigit,
        ("1",[])
    );
    (
        // idx 2
        // Lexer.stringof.03
        // two numbers
        "12",
        ["1";"2"],
        isdecimaldigit,
        ("12",[])
    );
    (
        // idx 3
        // Lexer.stringof.04
        // three numbers
        "123",
        ["1";"2";"3"],
        isdecimaldigit,
        ("123",[])
    );
    |]
[<Test>]
[<TestCase(0, TestName = "Lexer.stringof.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "Lexer.stringof.02")>]
[<TestCase(2, TestName = "Lexer.stringof.03")>]
[<TestCase(3, TestName = "Lexer.stringof.04")>]
let ``function lex.stringof`` idx =
    let (externalForm, _, _, _) = stringofValues.[idx]
    let (_, internalForm, _, _) = stringofValues.[idx]
    let (_, _, charTest, _ ) = stringofValues.[idx]
    let (_, _, _, (currentResult , restResult)) = stringofValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    let stringParser = stringof (ArithmeticExpressionEvaluator.ParserCombinator.some charTest)
    let (current,rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual (currentResult, current)
    Assert.AreEqual (restResult, rest)

//#endregion

//#region "integer tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private integerStringTypeValues : (string * string list * (token * string list))[] = [|
    (
        // idx 0
        // Lexer.integer.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.Lexer.integer function reads this
        (Integer "0",[])  // dummy value
    );
    (
        // idx 1
        // Lexer.integer.02
        // One decimal digit
        "1",
        ["1"],
        (Integer "1",[])
    );
    (
        // idx 2
        // Lexer.integer.03
        // Two decimal digits
        "10",
        ["1";"0"],
        (Integer "10",[])
    );
    (
        // idx 3
        // Lexer.integer.04
        // Multiple decimal digits
        "1234567890",
        ["1";"2";"3";"4";"5";"6";"7";"8";"9";"0"],
        (Integer "1234567890",[])
    );
    (
        // idx 4
        // Lexer.integer.05
        // a letter then a decimal digit
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a1",
        ["a";"1"],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 5
        // Lexer.integer.06
        // a decimal digit then a letter
        "1a",
        ["1";"a"],
        (Integer "1",["a"])
    );
    (
        // idx 6
        // Lexer.integer.07
        // a decimal digit then an operator
        "1+",
        ["1";"+"],
        (Integer "1",["+"])
    );
    (
        // idx 7
        // Lexer.integer.08
        // a decimal digit then a open paren
        "1(",
        ["1";"("],
        (Integer "1",["("])
    );
    (
        // idx 8
        // Lexer.integer.09
        // a decimal digit then a close paren
        "1)",
        ["1";")"],
        (Integer "1",[")"])
    );
    (
        // idx 9
        // Lexer.integer.010
        // a decimal digit then a space
        "1 ",
        ["1";" "],
        (Integer "1",[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "Lexer.integer.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "Lexer.integer.02")>]
[<TestCase(2, TestName = "Lexer.integer.03")>]
[<TestCase(3, TestName = "Lexer.integer.04")>]
[<TestCase(4, TestName = "Lexer.integer.05", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "Lexer.integer.06")>]
[<TestCase(6, TestName = "Lexer.integer.07")>]
[<TestCase(7, TestName = "Lexer.integer.08")>]
[<TestCase(8, TestName = "Lexer.integer.09")>]
[<TestCase(9, TestName = "Lexer.integer.010")>]
let ``function integer - type string`` idx =
    let (externalForm, _, _) = integerStringTypeValues.[idx]
    let (_, internalForm, _) = integerStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = integerStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.Lexer.integer
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "openParen tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private openParenStringTypeValues : (string * string list * (token * string list))[] = [|
    (
        // idx 0
        // Lexer.openParen.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.Lexer.openParen function reads this
        (Comma,[])  // dummy value
    );
    (
        // idx 1
        // Lexer.openParen.02
        // One open paren
        "(",
        ["("],
        (OpenParen,[])
    );
    (
        // idx 2
        // Lexer.openParen.03
        // Two open parens
        "((",
        ["(";"("],
        (OpenParen,["("])
    );
    (
        // idx 3
        // Lexer.openParen.04
        // a letter then a open paren
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a(",
        ["a";"("],
        (Comma,[]) // dummy value
    );
    (
        // idx 4
        // Lexer.openParen.05
        // a open paren then a letter
        "(a",
        ["(";"a"],
        (OpenParen,["a"])
    );
    (
        // idx 5
        // Lexer.openParen.06
        // a open paren then an operator
        "(+",
        ["(";"+"],
        (OpenParen,["+"])
    );
    (
        // idx 6
        // Lexer.openParen.07
        // a open paren then a decimal digit
        "(1",
        ["(";"1"],
        (OpenParen,["1"])
    );
    (
        // idx 7
        // Lexer.openParen.08
        // a open paren then a close paren
        "()",
        ["(";")"],
        (OpenParen, [")"])
    );
    (
        // idx 8
        // Lexer.openParen.09
        // a open paren then a space
        "( ",
        ["(";" "],
        (OpenParen,[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "Lexer.openParen.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "Lexer.openParen.02")>]
[<TestCase(2, TestName = "Lexer.openParen.03")>]
[<TestCase(3, TestName = "Lexer.openParen.04", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(4, TestName = "Lexer.openParen.05")>]
[<TestCase(5, TestName = "Lexer.openParen.06")>]
[<TestCase(6, TestName = "Lexer.openParen.07")>]
[<TestCase(7, TestName = "Lexer.openParen.08")>]
[<TestCase(8, TestName = "Lexer.openParen.09")>]
let ``function openParen - type string`` idx =
    let (externalForm, _, _) = openParenStringTypeValues.[idx]
    let (_, internalForm, _) = openParenStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = openParenStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.Lexer.openParen
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "closeParen tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private closeParenStringTypeValues : (string * string list * (token * string list))[] = [|
    (
        // idx 0
        // Lexer.closeParen.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.Lexer.closeParen function reads this
        (Comma,[])  // dummy value
    );
    (
        // idx 1
        // Lexer.closeParen.02
        // One close paren
        ")",
        [")"],
        (CloseParen,[])
    );
    (
        // idx 2
        // Lexer.closeParen.03
        // Two close parens
        "))",
        [")";")"],
        (CloseParen,[")"])
    );
    (
        // idx 3
        // Lexer.closeParen.04
        // a letter then a close paren
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a)",
        ["a";")"],
        (Comma,[]) // dummy value
    );
    (
        // idx 4
        // Lexer.closeParen.05
        // a close paren then a letter
        ")a",
        [")";"a"],
        (CloseParen,["a"])
    );
    (
        // idx 5
        // Lexer.closeParen.06
        // a close paren then an operator
        ")+",
        [")";"+"],
        (CloseParen,["+"])
    );
    (
        // idx 6
        // Lexer.closeParen.07
        // a close paren then a decimal digit
        ")1",
        [")";"1"],
        (CloseParen,["1"])
    );
    (
        // idx 7
        // Lexer.closeParen.08
        // a close paren then a open paren
        ")(",
        [")";"("],
        (CloseParen, ["("])
    );
    (
        // idx 8
        // Lexer.closeParen.09
        // a close paren then a space
        ") ",
        [")";" "],
        (CloseParen,[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "Lexer.closeParen.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "Lexer.closeParen.02")>]
[<TestCase(2, TestName = "Lexer.closeParen.03")>]
[<TestCase(3, TestName = "Lexer.closeParen.04", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(4, TestName = "Lexer.closeParen.05")>]
[<TestCase(5, TestName = "Lexer.closeParen.06")>]
[<TestCase(6, TestName = "Lexer.closeParen.07")>]
[<TestCase(7, TestName = "Lexer.closeParen.08")>]
[<TestCase(8, TestName = "Lexer.closeParen.09")>]
let ``function closeParen - type string`` idx =
    let (externalForm, _, _) = closeParenStringTypeValues.[idx]
    let (_, internalForm, _) = closeParenStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = closeParenStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.Lexer.closeParen
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "comma tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private commaStringTypeValues : (string * string list * (token * string list))[] = [|
    (
        // idx 0
        // Lexer.comma.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.Lexer.comma function reads this
        (OpenParen,[])  // dummy value
    );
    (
        // idx 1
        // Lexer.comma.02
        // One comma
        ",",
        [","],
        (Comma,[])
    );
    (
        // idx 2
        // Lexer.comma.03
        // Two commas
        ",,",
        [",";","],
        (Comma,[","])
    );
    (
        // idx 3
        // Lexer.comma.04
        // a letter then a comma
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a,",
        ["a";","],
        (OpenParen,[]) // dummy value
    );
    (
        // idx 4
        // Lexer.comma.05
        // a comma then a letter
        ",a",
        [",";"a"],
        (Comma,["a"])
    );
    (
        // idx 5
        // Lexer.comma.06
        // a comma then an operator
        ",+",
        [",";"+"],
        (Comma,["+"])
    );
    (
        // idx 6
        // Lexer.comma.07
        // a comma then a decimal digit
        ",1",
        [",";"1"],
        (Comma,["1"])
    );
    (
        // idx 7
        // Lexer.comma.08
        // a comma then a open paren
        ",(",
        [",";"("],
        (Comma, ["("])
    );
    (
        // idx 8
        // Lexer.comma.09
        // a comma then a space
        ", ",
        [",";" "],
        (Comma,[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "Lexer.comma.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "Lexer.comma.02")>]
[<TestCase(2, TestName = "Lexer.comma.03")>]
[<TestCase(3, TestName = "Lexer.comma.04", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(4, TestName = "Lexer.comma.05")>]
[<TestCase(5, TestName = "Lexer.comma.06")>]
[<TestCase(6, TestName = "Lexer.comma.07")>]
[<TestCase(7, TestName = "Lexer.comma.08")>]
[<TestCase(8, TestName = "Lexer.comma.09")>]
let ``function comma - type string`` idx =
    let (externalForm, _, _) = commaStringTypeValues.[idx]
    let (_, internalForm, _) = commaStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = commaStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.Lexer.comma
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "operator tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private operatorStringTypeValues : (string * string list * (token * string list))[] = [|
    (
        // idx 0
        // Lexer.operator.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.Lexer.operator function reads this
        (OpenParen,[])  // dummy value
    );
    (
        // idx 1
        // Lexer.operator.02
        // One star
        "*",
        ["*"],
        (Operator "*",[])
    );
    (
        // idx 2
        // Lexer.operator.03
        // One plus
        "+",
        ["+"],
        (Operator "+",[])
    );
    (
        // idx 3
        // Lexer.operator.04
        // One dash
        "-",
        ["-"],
        (Operator "-",[])
    );
    (
        // idx 4
        // Lexer.operator.05
        // One slash
        "/",
        ["/"],
        (Operator "/",[])
    );
    (
        // idx 5
        // Lexer.operator.06
        // Two stars
        "**",
        ["*";"*"],
        (Operator "*",["*"])
    );
    (
        // idx 6
        // Lexer.operator.07
        // One start then one plus
        "*+",
        ["*";"+"],
        (Operator "*",["+"])
    );
    (
        // idx 7
        // Lexer.operator.08
        // a letter then a star
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a*",
        ["a";"*"],
        (OpenParen,[]) // dummy value
    );
    (
        // idx 8
        // Lexer.operator.09
        // a star then a letter
        "*a",
        ["*";"a"],
        (Operator "*",["a"])
    );
    (
        // idx 9
        // Lexer.operator.010
        // a star then a decimal digit
        "*1",
        ["*";"1"],
        (Operator "*",["1"])
    );
    (
        // idx 10
        // Lexer.operator.011
        // a star then a open paren
        "*(",
        ["*";"("],
        (Operator "*", ["("])
    );
    (
        // idx 11
        // Lexer.operator.012
        // a star then a close paren
        "*)",
        ["*";")"],
        (Operator "*", [")"])
    );
    (
        // idx 12
        // Lexer.operator.013
        // a star then a space
        "* ",
        ["*";" "],
        (Operator "*",[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "Lexer.operator.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "Lexer.operator.02")>]
[<TestCase(2, TestName = "Lexer.operator.03")>]
[<TestCase(3, TestName = "Lexer.operator.04")>]
[<TestCase(4, TestName = "Lexer.operator.05")>]
[<TestCase(5, TestName = "Lexer.operator.06")>]
[<TestCase(6, TestName = "Lexer.operator.07")>]
[<TestCase(7, TestName = "Lexer.operator.08", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(8, TestName = "Lexer.operator.09")>]
[<TestCase(9, TestName = "Lexer.operator.010")>]
[<TestCase(10, TestName = "Lexer.operator.011")>]
[<TestCase(11, TestName = "Lexer.operator.012")>]
let ``function operator - type string`` idx =
    let (externalForm, _, _) = operatorStringTypeValues.[idx]
    let (_, internalForm, _) = operatorStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = operatorStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.Lexer.operator
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "rawtoken tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private rawtokenStringTypeValues : (string * string list * (token * string list))[] = [|
    (
        // idx 0
        // Lexer.rawtoken.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.Lexer.rawtoken function reads this
        (Comma,[])  // dummy value
    );
    (
        // idx 1
        // Lexer.rawtoken.02
        // One decimal digit
        "2",
        ["2"],
        (Integer "2",[])
    );
    (
        // idx 2
        // Lexer.rawtoken.03
        // One open paren
        "(",
        ["("],
        (OpenParen ,[])
    );
    (
        // idx 3
        // Lexer.rawtoken.04
        // One close paren
        ")",
        [")"],
        (CloseParen,[])
    );
    (
        // idx 4
        // Lexer.rawtoken.05
        // ONe comma
        ",",
        [","],
        (Comma,[])
    );
    (
        // idx 5
        // Lexer.rawtoken.06
        // One operator
        "*",
        ["*"],
        (Operator "*",[])
    );
    (
        // idx 6
        // Lexer.rawtoken.07
        // One space
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " ",
        [" "],
        (Comma,[])  // dummy value
    );

    (
        // idx 7
        // Lexer.rawtoken.08
        // Two decimal digits
        "23",
        ["2";"3"],
        (Integer "23",[])
    );
    (
        // idx 8
        // Lexer.rawtoken.09
        // One decimal digit then one open paren
        "2(",
        ["2";"("],
        (Integer "2",["("])
    );
    (
        // idx 9
        // Lexer.rawtoken.010
        // One decimal digit then one close paren
        "2)",
        ["2";")"],
        (Integer "2",[")"])
    );
    (
        // idx 10
        // Lexer.rawtoken.011
        // One decimal digit then one comma
        "2,",
        ["2";","],
        (Integer "2",[","])
    );
    (
        // idx 11
        // Lexer.rawtoken.012
        // One decimal digit then one operator
        "2*",
        ["2";"*"],
        (Integer "2",["*"])
    );
    (
        // idx 12
        // Lexer.rawtoken.013
        // One decimal digit then one space
        "2 ",
        ["2";" "],
        (Integer "2",[" "])
    );

    (
        // idx 13
        // Lexer.rawtoken.014
        // One open paren then one decimal digit
        "(2",
        ["(";"2"],
        (OpenParen,["2"])
    );
    (
        // idx 14
        // Lexer.rawtoken.015
        // Two open parens
        "((",
        ["(";"("],
        (OpenParen,["("])
    );
    (
        // idx 15
        // Lexer.rawtoken.016
        // One open paren then one close paren
        "()",
        ["(";")"],
        (OpenParen,[")"])
    );
    (
        // idx 16
        // Lexer.rawtoken.017
        // One open paren then one comma
        "(,",
        ["(";","],
        (OpenParen,[","])
    );
    (
        // idx 17
        // Lexer.rawtoken.018
        // One open paren then one operator
        "(*",
        ["(";"*"],
        (OpenParen,["*"])
    );
    (
        // idx 18
        // Lexer.rawtoken.019
        // One open paren then one space
        "( ",
        ["(";" "],
        (OpenParen,[" "])
    );

    (
        // idx 19
        // Lexer.rawtoken.020
        // One close paren then one decimal digit
        ")2",
        [")";"2"],
        (CloseParen,["2"])
    );
    (
        // idx 20
        // Lexer.rawtoken.021
        // One close paren then one open paren
        ")(",
        [")";"("],
        (CloseParen,["("])
    );
    (
        // idx 21
        // Lexer.rawtoken.022
        // Two close parens
        "))",
        [")";")"],
        (CloseParen,[")"])
    );
    (
        // idx 22
        // Lexer.rawtoken.023
        // One close paren then one comma
        "),",
        [")";","],
        (CloseParen,[","])
    );
    (
        // idx 23
        // Lexer.rawtoken.024
        // One close paren then one operator
        ")*",
        [")";"*"],
        (CloseParen,["*"])
    );
    (
        // idx 24
        // Lexer.rawtoken.025
        // One close paren then one space
        ") ",
        [")";" "],
        (CloseParen,[" "])
    );

    (
        // idx 25
        // Lexer.rawtoken.026
        // One comma then one decimal digit
        ",2",
        [",";"2"],
        (Comma,["2"])
    );
    (
        // idx 26
        // Lexer.rawtoken.027
        // One comma then one comma
        ",(",
        [",";"("],
        (Comma,["("])
    );
    (
        // idx 27
        // Lexer.rawtoken.028
        // One comma then one close paren
        ",)",
        [",";")"],
        (Comma,[")"])
    );
    (
        // idx 28
        // Lexer.rawtoken.029
        // Two commas
        ",,",
        [",";","],
        (Comma,[","])
    );
    (
        // idx 29
        // Lexer.rawtoken.030
        // One comma then one operator
        ",*",
        [",";"*"],
        (Comma,["*"])
    );
    (
        // idx 30
        // Lexer.rawtoken.031
        // One comma then one space
        ", ",
        [",";" "],
        (Comma,[" "])
    );

    (
        // idx 31
        // Lexer.rawtoken.032
        // One operator then one decimal digit
        "*2",
        ["*";"2"],
        (Operator "*",["2"])
    );
    (
        // idx 32
        // Lexer.rawtoken.033
        // One operator then one operator
        "*(",
        ["*";"("],
        (Operator "*",["("])
    );
    (
        // idx 33
        // Lexer.rawtoken.034
        // One operator then one close paren
        "*)",
        ["*";")"],
        (Operator "*",[")"])
    );
    (
        // idx 34
        // Lexer.rawtoken.035
        // One operator then one comma
        "*,",
        ["*";","],
        (Operator "*",[","])
    );
    (
        // idx 35
        // Lexer.rawtoken.036
        // Two operators
        "**",
        ["*";"*"],
        (Operator "*",["*"])
    );
    (
        // idx 36
        // Lexer.rawtoken.037
        // One operator then one space
        "* ",
        ["*";" "],
        (Operator "*",[" "])
    );

    (
        // idx 37
        // Lexer.rawtoken.038
        // One whitespace then one decimal digit
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " 2",
        [" ";"2"],
        (Comma,[])  // dummy value
    );
    (
        // idx 38
        // Lexer.rawtoken.039
        // One whitespace then one operator
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " (",
        [" ";"("],
        (Comma,[])  // dummy value
    );
    (
        // idx 39
        // Lexer.rawtoken.040
        // One whitespace then one close paren
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " )",
        [" ";")"],
        (Comma,[])  // dummy value
    );
    (
        // idx 40
        // Lexer.rawtoken.041
        // One whitespace then one comma
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " ,",
        [" ";","],
        (Comma,[])  // dummy value
    );
    (
        // idx 41
        // Lexer.rawtoken.042
        // One whitespace then one operator
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " *",
        [" ";"*"],
        (Comma,[])  // dummy value
    );
    (
        // idx 42
        // Lexer.rawtoken.043
        // Two whitewhitespace
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "  ",
        [" ";" "],
        (Comma,[])  // dummy value
    );


    |]

[<Test>]
[<TestCase(0, TestName = "Lexer.rawtoken.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "Lexer.rawtoken.02")>]
[<TestCase(2, TestName = "Lexer.rawtoken.03")>]
[<TestCase(3, TestName = "Lexer.rawtoken.04")>]
[<TestCase(4, TestName = "Lexer.rawtoken.05")>]
[<TestCase(5, TestName = "Lexer.rawtoken.06")>]
[<TestCase(6, TestName = "Lexer.rawtoken.07", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(7, TestName = "Lexer.rawtoken.08")>]
[<TestCase(8, TestName = "Lexer.rawtoken.09")>]
[<TestCase(9, TestName = "Lexer.rawtoken.010")>]
[<TestCase(10, TestName = "Lexer.rawtoken.011")>]
[<TestCase(11, TestName = "Lexer.rawtoken.012")>]
[<TestCase(12, TestName = "Lexer.rawtoken.013")>]
[<TestCase(13, TestName = "Lexer.rawtoken.014")>]
[<TestCase(14, TestName = "Lexer.rawtoken.015")>]
[<TestCase(15, TestName = "Lexer.rawtoken.016")>]
[<TestCase(16, TestName = "Lexer.rawtoken.017")>]
[<TestCase(17, TestName = "Lexer.rawtoken.018")>]
[<TestCase(18, TestName = "Lexer.rawtoken.019")>]
[<TestCase(19, TestName = "Lexer.rawtoken.020")>]
[<TestCase(20, TestName = "Lexer.rawtoken.021")>]
[<TestCase(21, TestName = "Lexer.rawtoken.022")>]
[<TestCase(22, TestName = "Lexer.rawtoken.023")>]
[<TestCase(23, TestName = "Lexer.rawtoken.024")>]
[<TestCase(24, TestName = "Lexer.rawtoken.025")>]
[<TestCase(25, TestName = "Lexer.rawtoken.026")>]
[<TestCase(26, TestName = "Lexer.rawtoken.027")>]
[<TestCase(27, TestName = "Lexer.rawtoken.028")>]
[<TestCase(28, TestName = "Lexer.rawtoken.029")>]
[<TestCase(29, TestName = "Lexer.rawtoken.030")>]
[<TestCase(30, TestName = "Lexer.rawtoken.031")>]
[<TestCase(31, TestName = "Lexer.rawtoken.032")>]
[<TestCase(32, TestName = "Lexer.rawtoken.033")>]
[<TestCase(33, TestName = "Lexer.rawtoken.034")>]
[<TestCase(34, TestName = "Lexer.rawtoken.035")>]
[<TestCase(35, TestName = "Lexer.rawtoken.036")>]
[<TestCase(36, TestName = "Lexer.rawtoken.037")>]
[<TestCase(37, TestName = "Lexer.rawtoken.038", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(38, TestName = "Lexer.rawtoken.039", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(39, TestName = "Lexer.rawtoken.040", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(40, TestName = "Lexer.rawtoken.041", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(41, TestName = "Lexer.rawtoken.042", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(42, TestName = "Lexer.rawtoken.043", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
let ``function rawtoken - type string`` idx =
    let (externalForm, _, _) = rawtokenStringTypeValues.[idx]
    let (_, internalForm, _) = rawtokenStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = rawtokenStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.Lexer.rawtoken
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "spacedtoken tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private spacedtokenStringTypeValues : (string * string list * (token * string list))[] = [|
    (
        // idx 0
        // Lexer.spacedtoken.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.Lexer.spacedtoken function reads this
        (Comma,[])  // dummy value
    );
    (
        // idx 1
        // Lexer.spacedtoken.02
        // One decimal digit
        "2",
        ["2"],
        (Integer "2",[])
    );
    (
        // idx 2
        // Lexer.spacedtoken.03
        // One open paren
        "(",
        ["("],
        (OpenParen ,[])
    );
    (
        // idx 3
        // Lexer.spacedtoken.04
        // One close paren
        ")",
        [")"],
        (CloseParen,[])
    );
    (
        // idx 4
        // Lexer.spacedtoken.05
        // ONe comma
        ",",
        [","],
        (Comma,[])
    );
    (
        // idx 5
        // Lexer.spacedtoken.06
        // One operator
        "*",
        ["*"],
        (Operator "*",[])
    );
    (
        // idx 6
        // Lexer.spacedtoken.07
        // One space
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // Throws error because a rawtoken is expected after space character
        // and there is no rawtoken.
        " ",
        [" "],
        (Comma,[])  // dummy value
    );

    (
        // idx 7
        // Lexer.spacedtoken.08
        // Two decimal digits
        "23",
        ["2";"3"],
        (Integer "23",[])
    );
    (
        // idx 8
        // Lexer.spacedtoken.09
        // One decimal digit then one open paren
        "2(",
        ["2";"("],
        (Integer "2",["("])
    );
    (
        // idx 9
        // Lexer.spacedtoken.010
        // One decimal digit then one close paren
        "2)",
        ["2";")"],
        (Integer "2",[")"])
    );
    (
        // idx 10
        // Lexer.spacedtoken.011
        // One decimal digit then one comma
        "2,",
        ["2";","],
        (Integer "2",[","])
    );
    (
        // idx 11
        // Lexer.spacedtoken.012
        // One decimal digit then one operator
        "2*",
        ["2";"*"],
        (Integer "2",["*"])
    );
    (
        // idx 12
        // Lexer.spacedtoken.013
        // One decimal digit then one space
        "2 ",
        ["2";" "],
        (Integer "2",[" "])
    );

    (
        // idx 13
        // Lexer.spacedtoken.014
        // One open paren then one decimal digit
        "(2",
        ["(";"2"],
        (OpenParen,["2"])
    );
    (
        // idx 14
        // Lexer.spacedtoken.015
        // Two open parens
        "((",
        ["(";"("],
        (OpenParen,["("])
    );
    (
        // idx 15
        // Lexer.spacedtoken.016
        // One open paren then one close paren
        "()",
        ["(";")"],
        (OpenParen,[")"])
    );
    (
        // idx 16
        // Lexer.spacedtoken.017
        // One open paren then one comma
        "(,",
        ["(";","],
        (OpenParen,[","])
    );
    (
        // idx 17
        // Lexer.spacedtoken.018
        // One open paren then one operator
        "(*",
        ["(";"*"],
        (OpenParen,["*"])
    );
    (
        // idx 18
        // Lexer.spacedtoken.019
        // One open paren then one space
        "( ",
        ["(";" "],
        (OpenParen,[" "])
    );

    (
        // idx 19
        // Lexer.spacedtoken.020
        // One close paren then one decimal digit
        ")2",
        [")";"2"],
        (CloseParen,["2"])
    );
    (
        // idx 20
        // Lexer.spacedtoken.021
        // One close paren then one open paren
        ")(",
        [")";"("],
        (CloseParen,["("])
    );
    (
        // idx 21
        // Lexer.spacedtoken.022
        // Two close parens
        "))",
        [")";")"],
        (CloseParen,[")"])
    );
    (
        // idx 22
        // Lexer.spacedtoken.023
        // One close paren then one comma
        "),",
        [")";","],
        (CloseParen,[","])
    );
    (
        // idx 23
        // Lexer.spacedtoken.024
        // One close paren then one operator
        ")*",
        [")";"*"],
        (CloseParen,["*"])
    );
    (
        // idx 24
        // Lexer.spacedtoken.025
        // One close paren then one space
        ") ",
        [")";" "],
        (CloseParen,[" "])
    );

    (
        // idx 25
        // Lexer.spacedtoken.026
        // One comma then one decimal digit
        ",2",
        [",";"2"],
        (Comma,["2"])
    );
    (
        // idx 26
        // Lexer.spacedtoken.027
        // One comma then one comma
        ",(",
        [",";"("],
        (Comma,["("])
    );
    (
        // idx 27
        // Lexer.spacedtoken.028
        // One comma then one close paren
        ",)",
        [",";")"],
        (Comma,[")"])
    );
    (
        // idx 28
        // Lexer.spacedtoken.029
        // Two commas
        ",,",
        [",";","],
        (Comma,[","])
    );
    (
        // idx 29
        // Lexer.spacedtoken.030
        // One comma then one operator
        ",*",
        [",";"*"],
        (Comma,["*"])
    );
    (
        // idx 30
        // Lexer.spacedtoken.031
        // One comma then one space
        ", ",
        [",";" "],
        (Comma,[" "])
    );

    (
        // idx 31
        // Lexer.spacedtoken.032
        // One operator then one decimal digit
        "*2",
        ["*";"2"],
        (Operator "*",["2"])
    );
    (
        // idx 32
        // Lexer.spacedtoken.033
        // One operator then one operator
        "*(",
        ["*";"("],
        (Operator "*",["("])
    );
    (
        // idx 33
        // Lexer.spacedtoken.034
        // One operator then one close paren
        "*)",
        ["*";")"],
        (Operator "*",[")"])
    );
    (
        // idx 34
        // Lexer.spacedtoken.035
        // One operator then one comma
        "*,",
        ["*";","],
        (Operator "*",[","])
    );
    (
        // idx 35
        // Lexer.spacedtoken.036
        // Two operators
        "**",
        ["*";"*"],
        (Operator "*",["*"])
    );
    (
        // idx 36
        // Lexer.spacedtoken.037
        // One operator then one space
        "* ",
        ["*";" "],
        (Operator "*",[" "])
    );

    (
        // idx 37
        // Lexer.spacedtoken.038
        // One whitespace then one decimal digit
        " 2",
        [" ";"2"],
        (Integer "2",[])
    );
    (
        // idx 38
        // Lexer.spacedtoken.039
        // One whitespace then one operator
        " (",
        [" ";"("],
        (OpenParen,[])
    );
    (
        // idx 39
        // Lexer.spacedtoken.040
        // One whitespace then one close paren
        " )",
        [" ";")"],
        (CloseParen,[])
    );
    (
        // idx 40
        // Lexer.spacedtoken.041
        // One whitespace then one comma
        " ,",
        [" ";","],
        (Comma,[])
    );
    (
        // idx 41
        // Lexer.spacedtoken.042
        // One whitespace then one operator
        " *",
        [" ";"*"],
        (Operator "*",[])
    );
    (
        // idx 42
        // Lexer.spacedtoken.043
        // Two whitewhitespace
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "  ",
        [" ";" "],
        (Comma,[])  // dummy value
    );


    |]

[<Test>]
[<TestCase(0, TestName = "Lexer.spacedtoken.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "Lexer.spacedtoken.02")>]
[<TestCase(2, TestName = "Lexer.spacedtoken.03")>]
[<TestCase(3, TestName = "Lexer.spacedtoken.04")>]
[<TestCase(4, TestName = "Lexer.spacedtoken.05")>]
[<TestCase(5, TestName = "Lexer.spacedtoken.06")>]
[<TestCase(6, TestName = "Lexer.spacedtoken.07", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(7, TestName = "Lexer.spacedtoken.08")>]
[<TestCase(8, TestName = "Lexer.spacedtoken.09")>]
[<TestCase(9, TestName = "Lexer.spacedtoken.010")>]
[<TestCase(10, TestName = "Lexer.spacedtoken.011")>]
[<TestCase(11, TestName = "Lexer.spacedtoken.012")>]
[<TestCase(12, TestName = "Lexer.spacedtoken.013")>]
[<TestCase(13, TestName = "Lexer.spacedtoken.014")>]
[<TestCase(14, TestName = "Lexer.spacedtoken.015")>]
[<TestCase(15, TestName = "Lexer.spacedtoken.016")>]
[<TestCase(16, TestName = "Lexer.spacedtoken.017")>]
[<TestCase(17, TestName = "Lexer.spacedtoken.018")>]
[<TestCase(18, TestName = "Lexer.spacedtoken.019")>]
[<TestCase(19, TestName = "Lexer.spacedtoken.020")>]
[<TestCase(20, TestName = "Lexer.spacedtoken.021")>]
[<TestCase(21, TestName = "Lexer.spacedtoken.022")>]
[<TestCase(22, TestName = "Lexer.spacedtoken.023")>]
[<TestCase(23, TestName = "Lexer.spacedtoken.024")>]
[<TestCase(24, TestName = "Lexer.spacedtoken.025")>]
[<TestCase(25, TestName = "Lexer.spacedtoken.026")>]
[<TestCase(26, TestName = "Lexer.spacedtoken.027")>]
[<TestCase(27, TestName = "Lexer.spacedtoken.028")>]
[<TestCase(28, TestName = "Lexer.spacedtoken.029")>]
[<TestCase(29, TestName = "Lexer.spacedtoken.030")>]
[<TestCase(30, TestName = "Lexer.spacedtoken.031")>]
[<TestCase(31, TestName = "Lexer.spacedtoken.032")>]
[<TestCase(32, TestName = "Lexer.spacedtoken.033")>]
[<TestCase(33, TestName = "Lexer.spacedtoken.034")>]
[<TestCase(34, TestName = "Lexer.spacedtoken.035")>]
[<TestCase(35, TestName = "Lexer.spacedtoken.036")>]
[<TestCase(36, TestName = "Lexer.spacedtoken.037")>]
[<TestCase(37, TestName = "Lexer.spacedtoken.038")>]
[<TestCase(38, TestName = "Lexer.spacedtoken.039")>]
[<TestCase(39, TestName = "Lexer.spacedtoken.040")>]
[<TestCase(40, TestName = "Lexer.spacedtoken.041")>]
[<TestCase(41, TestName = "Lexer.spacedtoken.042")>]
[<TestCase(42, TestName = "Lexer.spacedtoken.043", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
let ``function spacedtoken - type string`` idx =
    let (externalForm, _, _) = spacedtokenStringTypeValues.[idx]
    let (_, internalForm, _) = spacedtokenStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = spacedtokenStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.Lexer.spacedtoken
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "tokens tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private tokensStringTypeValues : (string * string list * (token list * string list))[] = [|
    (
        // idx 0
        // Lexer.tokens.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.Lexer.tokens function reads this
        ([],[])  // Notice no exception for empty list.
    );
    (
        // idx 1
        // Lexer.tokens.02
        // One decimal digit
        "2",
        ["2"],
        ([Integer "2"],[])
    );
    (
        // idx 2
        // Lexer.tokens.03
        // One open paren
        "(",
        ["("],
        ([OpenParen],[])
    );
    (
        // idx 3
        // Lexer.tokens.04
        // One close paren
        ")",
        [")"],
        ([CloseParen],[])
    );
    (
        // idx 4
        // Lexer.tokens.05
        // ONe comma
        ",",
        [","],
        ([Comma],[])
    );
    (
        // idx 5
        // Lexer.tokens.06
        // One operator
        "*",
        ["*"],
        ([Operator "*"],[])
    );
    (
        // idx 6
        // Lexer.tokens.07
        // One space
        " ",
        [" "],
        ([],[" "])  // Notice no exception for a single space.
    );

    (
        // idx 7
        // Lexer.tokens.08
        // Two decimal digits
        "23",
        ["2";"3"],
        ([Integer "23"],[])
    );
    (
        // idx 8
        // Lexer.tokens.09
        // One decimal digit then one open paren
        "2(",
        ["2";"("],
        ([Integer "2"; OpenParen],[])
    );
    (
        // idx 9
        // Lexer.tokens.010
        // One decimal digit then one close paren
        "2)",
        ["2";")"],
        ([Integer "2"; CloseParen],[])
    );
    (
        // idx 10
        // Lexer.tokens.011
        // One decimal digit then one comma
        "2,",
        ["2";","],
        ([Integer "2"; Comma],[])
    );
    (
        // idx 11
        // Lexer.tokens.012
        // One decimal digit then one operator
        "2*",
        ["2";"*"],
        ([Integer "2"; Operator "*"],[])
    );
    (
        // idx 12
        // Lexer.tokens.013
        // One decimal digit then one space
        "2 ",
        ["2";" "],
        ([Integer "2"],[" "])
    );

    (
        // idx 13
        // Lexer.tokens.014
        // One open paren then one decimal digit
        "(2",
        ["(";"2"],
        ([OpenParen; Integer "2"],[])
    );
    (
        // idx 14
        // Lexer.tokens.015
        // Two open parens
        "((",
        ["(";"("],
        ([OpenParen; OpenParen],[])
    );
    (
        // idx 15
        // Lexer.tokens.016
        // One open paren then one close paren
        "()",
        ["(";")"],
        ([OpenParen; CloseParen],[])
    );
    (
        // idx 16
        // Lexer.tokens.017
        // One open paren then one comma
        "(,",
        ["(";","],
        ([OpenParen; Comma],[])
    );
    (
        // idx 17
        // Lexer.tokens.018
        // One open paren then one operator
        "(*",
        ["(";"*"],
        ([OpenParen; Operator "*"],[])
    );
    (
        // idx 18
        // Lexer.tokens.019
        // One open paren then one space
        "( ",
        ["(";" "],
        ([OpenParen],[" "])
    );

    (
        // idx 19
        // Lexer.tokens.020
        // One close paren then one decimal digit
        ")2",
        [")";"2"],
        ([CloseParen; Integer "2"],[])
    );
    (
        // idx 20
        // Lexer.tokens.021
        // One close paren then one open paren
        ")(",
        [")";"("],
        ([CloseParen; OpenParen],[])
    );
    (
        // idx 21
        // Lexer.tokens.022
        // Two close parens
        "))",
        [")";")"],
        ([CloseParen; CloseParen],[])
    );
    (
        // idx 22
        // Lexer.tokens.023
        // One close paren then one comma
        "),",
        [")";","],
        ([CloseParen; Comma],[])
    );
    (
        // idx 23
        // Lexer.tokens.024
        // One close paren then one operator
        ")*",
        [")";"*"],
        ([CloseParen; Operator "*"],[])
    );
    (
        // idx 24
        // Lexer.tokens.025
        // One close paren then one space
        ") ",
        [")";" "],
        ([CloseParen],[" "])
    );

    (
        // idx 25
        // Lexer.tokens.026
        // One comma then one decimal digit
        ",2",
        [",";"2"],
        ([Comma; Integer "2"],[])
    );
    (
        // idx 26
        // Lexer.tokens.027
        // One comma then one comma
        ",(",
        [",";"("],
        ([Comma; OpenParen],[])
    );
    (
        // idx 27
        // Lexer.tokens.028
        // One comma then one close paren
        ",)",
        [",";")"],
        ([Comma; CloseParen],[])
    );
    (
        // idx 28
        // Lexer.tokens.029
        // Two commas
        ",,",
        [",";","],
        ([Comma; Comma],[])
    );
    (
        // idx 29
        // Lexer.tokens.030
        // One comma then one operator
        ",*",
        [",";"*"],
        ([Comma; Operator "*"],[])
    );
    (
        // idx 30
        // Lexer.tokens.031
        // One comma then one space
        ", ",
        [",";" "],
        ([Comma],[" "])
    );

    (
        // idx 31
        // Lexer.tokens.032
        // One operator then one decimal digit
        "*2",
        ["*";"2"],
        ([Operator "*"; Integer "2"],[])
    );
    (
        // idx 32
        // Lexer.tokens.033
        // One operator then one operator
        "*(",
        ["*";"("],
        ([Operator "*"; OpenParen],[])
    );
    (
        // idx 33
        // Lexer.tokens.034
        // One operator then one close paren
        "*)",
        ["*";")"],
        ([Operator "*"; CloseParen],[])
    );
    (
        // idx 34
        // Lexer.tokens.035
        // One operator then one comma
        "*,",
        ["*";","],
        ([Operator "*"; Comma],[])
    );
    (
        // idx 35
        // Lexer.tokens.036
        // Two operators
        "**",
        ["*";"*"],
        ([Operator "*"; Operator "*"],[])
    );
    (
        // idx 36
        // Lexer.tokens.037
        // One operator then one space
        "* ",
        ["*";" "],
        ([Operator "*"],[" "])
    );

    (
        // idx 37
        // Lexer.tokens.038
        // One whitespace then one decimal digit
        " 2",
        [" ";"2"],
        ([Integer "2"],[])
    );
    (
        // idx 38
        // Lexer.tokens.039
        // One whitespace then one operator
        " (",
        [" ";"("],
        ([OpenParen],[])
    );
    (
        // idx 39
        // Lexer.tokens.040
        // One whitespace then one close paren
        " )",
        [" ";")"],
        ([CloseParen],[])
    );
    (
        // idx 40
        // Lexer.tokens.041
        // One whitespace then one comma
        " ,",
        [" ";","],
        ([Comma],[])
    );
    (
        // idx 41
        // Lexer.tokens.042
        // One whitespace then one operator
        " *",
        [" ";"*"],
        ([Operator "*"],[])
    );
    (
        // idx 42
        // Lexer.tokens.043
        // Two whitewhitespace
        "  ",
        [" ";" "],
        ([],[" "; " "])
    );
    (
        // idx 43
        // Lexer.tokens.044
        // One alpha character
        "a",
        ["a"],
        ([],["a"])
    );
    (
        // idx 44
        // Lexer.tokens.045
        // One alpha character
        "1 ",
        ["1"; " "],
        ([Integer "1"],[" "])  // Notice: tokens function does not process trailing whitespace.
    );
    |]

[<Test>]
[<TestCase(0, TestName = "Lexer.tokens.01")>]
[<TestCase(1, TestName = "Lexer.tokens.02")>]
[<TestCase(2, TestName = "Lexer.tokens.03")>]
[<TestCase(3, TestName = "Lexer.tokens.04")>]
[<TestCase(4, TestName = "Lexer.tokens.05")>]
[<TestCase(5, TestName = "Lexer.tokens.06")>]
[<TestCase(6, TestName = "Lexer.tokens.07")>]
[<TestCase(7, TestName = "Lexer.tokens.08")>]
[<TestCase(8, TestName = "Lexer.tokens.09")>]
[<TestCase(9, TestName = "Lexer.tokens.010")>]
[<TestCase(10, TestName = "Lexer.tokens.011")>]
[<TestCase(11, TestName = "Lexer.tokens.012")>]
[<TestCase(12, TestName = "Lexer.tokens.013")>]
[<TestCase(13, TestName = "Lexer.tokens.014")>]
[<TestCase(14, TestName = "Lexer.tokens.015")>]
[<TestCase(15, TestName = "Lexer.tokens.016")>]
[<TestCase(16, TestName = "Lexer.tokens.017")>]
[<TestCase(17, TestName = "Lexer.tokens.018")>]
[<TestCase(18, TestName = "Lexer.tokens.019")>]
[<TestCase(19, TestName = "Lexer.tokens.020")>]
[<TestCase(20, TestName = "Lexer.tokens.021")>]
[<TestCase(21, TestName = "Lexer.tokens.022")>]
[<TestCase(22, TestName = "Lexer.tokens.023")>]
[<TestCase(23, TestName = "Lexer.tokens.024")>]
[<TestCase(24, TestName = "Lexer.tokens.025")>]
[<TestCase(25, TestName = "Lexer.tokens.026")>]
[<TestCase(26, TestName = "Lexer.tokens.027")>]
[<TestCase(27, TestName = "Lexer.tokens.028")>]
[<TestCase(28, TestName = "Lexer.tokens.029")>]
[<TestCase(29, TestName = "Lexer.tokens.030")>]
[<TestCase(30, TestName = "Lexer.tokens.031")>]
[<TestCase(31, TestName = "Lexer.tokens.032")>]
[<TestCase(32, TestName = "Lexer.tokens.033")>]
[<TestCase(33, TestName = "Lexer.tokens.034")>]
[<TestCase(34, TestName = "Lexer.tokens.035")>]
[<TestCase(35, TestName = "Lexer.tokens.036")>]
[<TestCase(36, TestName = "Lexer.tokens.037")>]
[<TestCase(37, TestName = "Lexer.tokens.038")>]
[<TestCase(38, TestName = "Lexer.tokens.039")>]
[<TestCase(39, TestName = "Lexer.tokens.040")>]
[<TestCase(40, TestName = "Lexer.tokens.041")>]
[<TestCase(41, TestName = "Lexer.tokens.042")>]
[<TestCase(42, TestName = "Lexer.tokens.043")>]
[<TestCase(43, TestName = "Lexer.tokens.044")>]
[<TestCase(44, TestName = "Lexer.tokens.045")>]
let ``function tokens - type string`` idx =
    let (externalForm, _, _) = tokensStringTypeValues.[idx]
    let (_, internalForm, _) = tokensStringTypeValues.[idx]
    let (_, _, (currentResult , restResult)) = tokensStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token list * string list = ArithmeticExpressionEvaluator.Lexer.tokens
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "lex tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private lexStringTypeValues : (string * string list * token list)[] = [|
    (
        // idx 0
        // Lexer.lex.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.Lexer.lex function reads this
        []  // Notice no exception for empty list.
    );
    (
        // idx 1
        // Lexer.lex.02
        // One decimal digit
        "2",
        ["2"],
        [Integer "2"]
    );
    (
        // idx 2
        // Lexer.lex.03
        // One open paren
        "(",
        ["("],
        [OpenParen]
    );
    (
        // idx 3
        // Lexer.lex.04
        // One close paren
        ")",
        [")"],
        [CloseParen]
    );
    (
        // idx 4
        // Lexer.lex.05
        // ONe comma
        ",",
        [","],
        [Comma]
    );
    (
        // idx 5
        // Lexer.lex.06
        // One operator
        "*",
        ["*"],
        [Operator "*"]
    );
    (
        // idx 6
        // Lexer.lex.07
        // One space
        " ",
        [" "],
        []  // Notice no exception for a single space.
    );

    (
        // idx 7
        // Lexer.lex.08
        // Two decimal digits
        "23",
        ["2";"3"],
        [Integer "23"]
    );
    (
        // idx 8
        // Lexer.lex.09
        // One decimal digit then one open paren
        "2(",
        ["2";"("],
        [Integer "2"; OpenParen]
    );
    (
        // idx 9
        // Lexer.lex.010
        // One decimal digit then one close paren
        "2)",
        ["2";")"],
        [Integer "2"; CloseParen]
    );
    (
        // idx 10
        // Lexer.lex.011
        // One decimal digit then one comma
        "2,",
        ["2";","],
        [Integer "2"; Comma]
    );
    (
        // idx 11
        // Lexer.lex.012
        // One decimal digit then one operator
        "2*",
        ["2";"*"],
        [Integer "2"; Operator "*"]
    );
    (
        // idx 12
        // Lexer.lex.013
        // One decimal digit then one space
        "2 ",
        ["2";" "],
        [Integer "2"]
    );

    (
        // idx 13
        // Lexer.lex.014
        // One open paren then one decimal digit
        "(2",
        ["(";"2"],
        [OpenParen; Integer "2"]
    );
    (
        // idx 14
        // Lexer.lex.015
        // Two open parens
        "((",
        ["(";"("],
        [OpenParen; OpenParen]
    );
    (
        // idx 15
        // Lexer.lex.016
        // One open paren then one close paren
        "()",
        ["(";")"],
        [OpenParen; CloseParen]
    );
    (
        // idx 16
        // Lexer.lex.017
        // One open paren then one comma
        "(,",
        ["(";","],
        [OpenParen; Comma]
    );
    (
        // idx 17
        // Lexer.lex.018
        // One open paren then one operator
        "(*",
        ["(";"*"],
        [OpenParen; Operator "*"]
    );
    (
        // idx 18
        // Lexer.lex.019
        // One open paren then one space
        "( ",
        ["(";" "],
        [OpenParen]
    );

    (
        // idx 19
        // Lexer.lex.020
        // One close paren then one decimal digit
        ")2",
        [")";"2"],
        [CloseParen; Integer "2"]
    );
    (
        // idx 20
        // Lexer.lex.021
        // One close paren then one open paren
        ")(",
        [")";"("],
        [CloseParen; OpenParen]
    );
    (
        // idx 21
        // Lexer.lex.022
        // Two close parens
        "))",
        [")";")"],
        [CloseParen; CloseParen]
    );
    (
        // idx 22
        // Lexer.lex.023
        // One close paren then one comma
        "),",
        [")";","],
        [CloseParen; Comma]
    );
    (
        // idx 23
        // Lexer.lex.024
        // One close paren then one operator
        ")*",
        [")";"*"],
        [CloseParen; Operator "*"]
    );
    (
        // idx 24
        // Lexer.lex.025
        // One close paren then one space
        ") ",
        [")";" "],
        [CloseParen]
    );

    (
        // idx 25
        // Lexer.lex.026
        // One comma then one decimal digit
        ",2",
        [",";"2"],
        [Comma; Integer "2"]
    );
    (
        // idx 26
        // Lexer.lex.027
        // One comma then one comma
        ",(",
        [",";"("],
        [Comma; OpenParen]
    );
    (
        // idx 27
        // Lexer.lex.028
        // One comma then one close paren
        ",)",
        [",";")"],
        [Comma; CloseParen]
    );
    (
        // idx 28
        // Lexer.lex.029
        // Two commas
        ",,",
        [",";","],
        [Comma; Comma]
    );
    (
        // idx 29
        // Lexer.lex.030
        // One comma then one operator
        ",*",
        [",";"*"],
        [Comma; Operator "*"]
    );
    (
        // idx 30
        // Lexer.lex.031
        // One comma then one space
        ", ",
        [",";" "],
        [Comma]
    );

    (
        // idx 31
        // Lexer.lex.032
        // One operator then one decimal digit
        "*2",
        ["*";"2"],
        [Operator "*"; Integer "2"]
    );
    (
        // idx 32
        // Lexer.lex.033
        // One operator then one operator
        "*(",
        ["*";"("],
        [Operator "*"; OpenParen]
    );
    (
        // idx 33
        // Lexer.lex.034
        // One operator then one close paren
        "*)",
        ["*";")"],
        [Operator "*"; CloseParen]
    );
    (
        // idx 34
        // Lexer.lex.035
        // One operator then one comma
        "*,",
        ["*";","],
        [Operator "*"; Comma]
    );
    (
        // idx 35
        // Lexer.lex.036
        // Two operators
        "**",
        ["*";"*"],
        [Operator "*"; Operator "*"]
    );
    (
        // idx 36
        // Lexer.lex.037
        // One operator then one space
        "* ",
        ["*";" "],
        [Operator "*"]
    );

    (
        // idx 37
        // Lexer.lex.038
        // One whitespace then one decimal digit
        " 2",
        [" ";"2"],
        [Integer "2"]
    );
    (
        // idx 38
        // Lexer.lex.039
        // One whitespace then one operator
        " (",
        [" ";"("],
        [OpenParen]
    );
    (
        // idx 39
        // Lexer.lex.040
        // One whitespace then one close paren
        " )",
        [" ";")"],
        [CloseParen]
    );
    (
        // idx 40
        // Lexer.lex.041
        // One whitespace then one comma
        " ,",
        [" ";","],
        [Comma]
    );
    (
        // idx 41
        // Lexer.lex.042
        // One whitespace then one operator
        " *",
        [" ";"*"],
        [Operator "*"]
    );
    (
        // idx 42
        // Lexer.lex.043
        // Two whitewhitespace
        "  ",
        [" ";" "],
        []
    );
    (
        // idx 43
        // Lexer.lex.044
        // One alpha character
        // throws System.Exception "Unparsed input"
        "a",
        ["a"],
        []
    );
    (
        // idx 44
        // Lexer.lex.045
        // One alpha character
        "1 ",
        ["1"; " "],
        [Integer "1"]
    );
    |]

[<Test>]
[<TestCase(0, TestName = "Lexer.lex.01")>]
[<TestCase(1, TestName = "Lexer.lex.02")>]
[<TestCase(2, TestName = "Lexer.lex.03")>]
[<TestCase(3, TestName = "Lexer.lex.04")>]
[<TestCase(4, TestName = "Lexer.lex.05")>]
[<TestCase(5, TestName = "Lexer.lex.06")>]
[<TestCase(6, TestName = "Lexer.lex.07")>]
[<TestCase(7, TestName = "Lexer.lex.08")>]
[<TestCase(8, TestName = "Lexer.lex.09")>]
[<TestCase(9, TestName = "Lexer.lex.010")>]
[<TestCase(10, TestName = "Lexer.lex.011")>]
[<TestCase(11, TestName = "Lexer.lex.012")>]
[<TestCase(12, TestName = "Lexer.lex.013")>]
[<TestCase(13, TestName = "Lexer.lex.014")>]
[<TestCase(14, TestName = "Lexer.lex.015")>]
[<TestCase(15, TestName = "Lexer.lex.016")>]
[<TestCase(16, TestName = "Lexer.lex.017")>]
[<TestCase(17, TestName = "Lexer.lex.018")>]
[<TestCase(18, TestName = "Lexer.lex.019")>]
[<TestCase(19, TestName = "Lexer.lex.020")>]
[<TestCase(20, TestName = "Lexer.lex.021")>]
[<TestCase(21, TestName = "Lexer.lex.022")>]
[<TestCase(22, TestName = "Lexer.lex.023")>]
[<TestCase(23, TestName = "Lexer.lex.024")>]
[<TestCase(24, TestName = "Lexer.lex.025")>]
[<TestCase(25, TestName = "Lexer.lex.026")>]
[<TestCase(26, TestName = "Lexer.lex.027")>]
[<TestCase(27, TestName = "Lexer.lex.028")>]
[<TestCase(28, TestName = "Lexer.lex.029")>]
[<TestCase(29, TestName = "Lexer.lex.030")>]
[<TestCase(30, TestName = "Lexer.lex.031")>]
[<TestCase(31, TestName = "Lexer.lex.032")>]
[<TestCase(32, TestName = "Lexer.lex.033")>]
[<TestCase(33, TestName = "Lexer.lex.034")>]
[<TestCase(34, TestName = "Lexer.lex.035")>]
[<TestCase(35, TestName = "Lexer.lex.036")>]
[<TestCase(36, TestName = "Lexer.lex.037")>]
[<TestCase(37, TestName = "Lexer.lex.038")>]
[<TestCase(38, TestName = "Lexer.lex.039")>]
[<TestCase(39, TestName = "Lexer.lex.040")>]
[<TestCase(40, TestName = "Lexer.lex.041")>]
[<TestCase(41, TestName = "Lexer.lex.042")>]
[<TestCase(42, TestName = "Lexer.lex.043")>]
[<TestCase(43, TestName = "Lexer.lex.044", ExpectedException=typeof<System.Exception>, ExpectedMessage = "Unparsed input")>]
[<TestCase(44, TestName = "Lexer.lex.045")>]
let ``function lex - type string`` idx =
    let (externalForm, _, _) = lexStringTypeValues.[idx]
    let (_, internalForm, _) = lexStringTypeValues.[idx]
    let (_, _, currentResult) = lexStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token list = ArithmeticExpressionEvaluator.Lexer.lex
    let current = stringParser internalForm
//    printfn "expected result: %A" currentResult
//    printfn "function result: %A" current
    Assert.AreEqual(current, currentResult)

//#endregion


