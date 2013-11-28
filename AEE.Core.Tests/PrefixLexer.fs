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

module ArithmeticExpressionEvaluator.PrefixLexer.Tests

open ArithmeticExpressionEvaluator.PrefixLexer

open NUnit.Framework

//#region "iswhitespace tests"

let private iswhitespaceValues : (string * bool)[] = [|
    (
        // idx 0
        // PrefixLexer.iswhitespace.01
        // No input
        // System.IndexOutOfRangeException - "Index was outside the bounds of the array."
        // The error is because the function uses an array and there is no char to covert to an index into the array.
        "",
        false
    );
    (
        // idx 1
        // PrefixLexer.iswhitespace.02
        // one space
        " ",
        true
    );
    (
        // idx 2
        // PrefixLexer.iswhitespace.03
        // one tab
        "\t",
        true
    );
    (
        // idx 3
        // PrefixLexer.iswhitespace.04
        // one newline
        "\n",
        true
    );
    (
        // idx 4
        // PrefixLexer.iswhitespace.05
        // one carriage return
        "\r",
        true
    );
    (
        // idx 5
        // PrefixLexer.iswhitespace.06
        // one space then one letter
        " a",
        true
    );
    (
        // idx 6
        // PrefixLexer.iswhitespace.07
        // one tab then one letter
        "\ta",
        true
    );
    (
        // idx 7
        // PrefixLexer.iswhitespace.08
        // one newline then one letter
        "\na",
        true
    );
    (
        // idx 8
        // PrefixLexer.iswhitespace.09
        // one carriage return then one letter
        "\ra",
        true
    );
    (
        // idx 9
        // PrefixLexer.iswhitespace.10
        // one letter
        "a",
        false
    );
    (
        // idx 10
        // PrefixLexer.iswhitespace.11
        // one decimal digit
        "1",
        false
    );
    (
        // idx 11
        // PrefixLexer.iswhitespace.12
        // one letter and one space
        "a ",
        false
    );
    |]
[<Test>]
[<TestCase(0, TestName = "PrefixLexer.iswhitespace.01", ExpectedException=typeof<System.IndexOutOfRangeException>, ExpectedMessage = "Index was outside the bounds of the array.")>]
[<TestCase(1, TestName = "PrefixLexer.iswhitespace.02")>]
[<TestCase(2, TestName = "PrefixLexer.iswhitespace.03")>]
[<TestCase(3, TestName = "PrefixLexer.iswhitespace.04")>]
[<TestCase(4, TestName = "PrefixLexer.iswhitespace.05")>]
[<TestCase(5, TestName = "PrefixLexer.iswhitespace.06")>]
[<TestCase(6, TestName = "PrefixLexer.iswhitespace.07")>]
[<TestCase(7, TestName = "PrefixLexer.iswhitespace.08")>]
[<TestCase(8, TestName = "PrefixLexer.iswhitespace.09")>]
[<TestCase(9, TestName = "PrefixLexer.iswhitespace.010")>]
[<TestCase(10, TestName = "PrefixLexer.iswhitespace.011")>]
[<TestCase(11, TestName = "PrefixLexer.iswhitespace.012")>]
let ``function lex.iswhitespace`` idx =
    let (input, _) = iswhitespaceValues.[idx]
    let (_, result) = iswhitespaceValues.[idx]

    let functionResult = ArithmeticExpressionEvaluator.PrefixLexer.iswhitespace input
//    printfn "expected result: %A" result
//    printfn "function result: %A" functionResult
    Assert.AreEqual (result, functionResult)

//#endregion

//#region "isparen tests"

let private isparenValues : (string * bool)[] = [|
    (
        // idx 0
        // PrefixLexer.isparen.01
        // No input
        // System.IndexOutOfRangeException - "Index was outside the bounds of the array."
        // The error is because the function uses an array and there is no char to covert to an index into the array.
        "",
        false
    );
    (
        // idx 1
        // PrefixLexer.isparen.02
        // one open paren
        "(",
        true
    );
    (
        // idx 2
        // PrefixLexer.isparen.03
        // one close paren
        ")",
        true
    );
    (
        // idx 3
        // PrefixLexer.isparen.04
        // one open paren then one letter
        "(a",
        true
    );
    (
        // idx 4
        // PrefixLexer.isparen.05
        // one close paren then one letter
        ")a",
        true
    );
    (
        // idx 5
        // PrefixLexer.isparen.06
        // one letter
        "a",
        false
    );
    (
        // idx 6
        // PrefixLexer.isparen.07
        // one decimal digit
        "1",
        false
    );
    (
        // idx 7
        // PrefixLexer.isparen.08
        // one letter and one open paren
        "a(",
        false
    );
    |]
[<Test>]
[<TestCase(0, TestName = "PrefixLexer.isparen.01", ExpectedException=typeof<System.IndexOutOfRangeException>, ExpectedMessage = "Index was outside the bounds of the array.")>]
[<TestCase(1, TestName = "PrefixLexer.isparen.02")>]
[<TestCase(2, TestName = "PrefixLexer.isparen.03")>]
[<TestCase(3, TestName = "PrefixLexer.isparen.04")>]
[<TestCase(4, TestName = "PrefixLexer.isparen.05")>]
[<TestCase(5, TestName = "PrefixLexer.isparen.06")>]
[<TestCase(6, TestName = "PrefixLexer.isparen.07")>]
[<TestCase(7, TestName = "PrefixLexer.isparen.08")>]
let ``function lex.isparen`` idx =
    let (input, _) = isparenValues.[idx]
    let (_, result) = isparenValues.[idx]

    let functionResult = ArithmeticExpressionEvaluator.PrefixLexer.isparen input
//    printfn "expected result: %A" result
//    printfn "function result: %A" functionResult
    Assert.AreEqual (result, functionResult)

//#endregion

//#region "isoperator tests"

let private isoperatorValues : (string * bool)[] = [|
    (
        // idx 0
        // PrefixLexer.isoperator.01
        // No input
        // System.IndexOutOfRangeException - "Index was outside the bounds of the array."
        // The error is because the function uses an array and there is no char to covert to an index into the array.
        "",
        false
    );
    (
        // idx 1
        // PrefixLexer.isoperator.02
        // one star
        "*",
        true
    );
    (
        // idx 2
        // PrefixLexer.isoperator.03
        // one plus
        "+",
        true
    );
    (
        // idx 3
        // PrefixLexer.isoperator.04
        // one dash
        "-",
        true
    );
    (
        // idx 4
        // PrefixLexer.isoperator.05
        // one slash
        "/",
        true
    );
    (
        // idx 5
        // PrefixLexer.isoperator.06
        // one star then one letter
        "*a",
        true
    );
    (
        // idx 6
        // PrefixLexer.isoperator.07
        // one plus then one letter
        "+a",
        true
    );
    (
        // idx 7
        // PrefixLexer.isoperator.08
        // one dash then one letter
        "-a",
        true
    );
    (
        // idx 8
        // PrefixLexer.isoperator.09
        // one slash then one letter
        "/a",
        true
    );
    (
        // idx 9
        // PrefixLexer.isoperator.10
        // one letter
        "a",
        false
    );
    (
        // idx 10
        // PrefixLexer.isoperator.11
        // one decimal digit
        "1",
        false
    );
    (
        // idx 11
        // PrefixLexer.isoperator.12
        // one letter and one star
        "a*",
        false
    );
    |]
[<Test>]
[<TestCase(0, TestName = "PrefixLexer.isoperator.01", ExpectedException=typeof<System.IndexOutOfRangeException>, ExpectedMessage = "Index was outside the bounds of the array.")>]
[<TestCase(1, TestName = "PrefixLexer.isoperator.02")>]
[<TestCase(2, TestName = "PrefixLexer.isoperator.03")>]
[<TestCase(3, TestName = "PrefixLexer.isoperator.04")>]
[<TestCase(4, TestName = "PrefixLexer.isoperator.05")>]
[<TestCase(5, TestName = "PrefixLexer.isoperator.06")>]
[<TestCase(6, TestName = "PrefixLexer.isoperator.07")>]
[<TestCase(7, TestName = "PrefixLexer.isoperator.08")>]
[<TestCase(8, TestName = "PrefixLexer.isoperator.09")>]
[<TestCase(9, TestName = "PrefixLexer.isoperator.010")>]
[<TestCase(10, TestName = "PrefixLexer.isoperator.011")>]
[<TestCase(11, TestName = "PrefixLexer.isoperator.012")>]
let ``function lex.isoperator`` idx =
    let (input, _) = isoperatorValues.[idx]
    let (_, result) = isoperatorValues.[idx]

    let functionResult = ArithmeticExpressionEvaluator.PrefixLexer.isoperator input
//    printfn "expected result: %A" result
//    printfn "function result: %A" functionResult
    Assert.AreEqual (result, functionResult)

//#endregion

//#region "isdecimaldigit tests"

let private isdecimaldigitValues : (string * bool)[] = [|
    (
        // idx 0
        // PrefixLexer.isdecimaldigit.001
        // No input
        // System.IndexOutOfRangeException - "Index was outside the bounds of the array."
        // The error is because the function uses an array and there is no char to covert to an index into the array.
        "",
        false
    );
    (
        // idx 1
        // PrefixLexer.isdecimaldigit.002
        // one one character
        "1",
        true
    );
    (
        // idx 2
        // PrefixLexer.isdecimaldigit.003
        // one two character
        "2",
        true
    );
    (
        // idx 3
        // PrefixLexer.isdecimaldigit.004
        // one three character
        "3",
        true
    );
    (
        // idx 4
        // PrefixLexer.isdecimaldigit.005
        // one four character
        "4",
        true
    );
    (
        // idx 5
        // PrefixLexer.isdecimaldigit.006
        // one five character
        "5",
        true
    );
    (
        // idx 6
        // PrefixLexer.isdecimaldigit.007
        // one six character
        "6",
        true
    );
    (
        // idx 7
        // PrefixLexer.isdecimaldigit.008
        // one seven character
        "7",
        true
    );
    (
        // idx 8
        // PrefixLexer.isdecimaldigit.009
        // one eight character
        "8",
        true
    );
    (
        // idx 9
        // PrefixLexer.isdecimaldigit.010
        // one nine character
        "9",
        true
    );
    (
        // idx 10
        // PrefixLexer.isdecimaldigit.011
        // one zero character
        "0",
        true
    );
    (
        // idx 11
        // PrefixLexer.isdecimaldigit.012
        // one one character then one letter
        "1a",
        true
    );
    (
        // idx 12
        // PrefixLexer.isdecimaldigit.013
        // one two character then one letter
        "2a",
        true
    );
    (
        // idx 13
        // PrefixLexer.isdecimaldigit.014
        // one three character then one letter
        "3a",
        true
    );
    (
        // idx 14
        // PrefixLexer.isdecimaldigit.015
        // one four character then one letter
        "4a",
        true
    );
    (
        // idx 15
        // PrefixLexer.isdecimaldigit.016
        // one five character then one letter
        "5a",
        true
    );
    (
        // idx 16
        // PrefixLexer.isdecimaldigit.017
        // one six character then one letter
        "6a",
        true
    );
    (
        // idx 17
        // PrefixLexer.isdecimaldigit.018
        // one seven character then one letter
        "7a",
        true
    );
    (
        // idx 18
        // PrefixLexer.isdecimaldigit.019
        // one eight character then one letter
        "8a",
        true
    );
    (
        // idx 19
        // PrefixLexer.isdecimaldigit.020
        // one nine character then one letter
        "9a",
        true
    );
    (
        // idx 20
        // PrefixLexer.isdecimaldigit.021
        // one zero character then one letter
        "0a",
        true
    );
    (
        // idx 21
        // PrefixLexer.isdecimaldigit.22
        // one letter
        "a",
        false
    );
    (
        // idx 22
        // PrefixLexer.isdecimaldigit.23
        // one star
        "*",
        false
    );
    (
        // idx 23
        // PrefixLexer.isdecimaldigit.24
        // one letter and one one character
        "a1",
        false
    );
    |]
[<Test>]
[<TestCase(0, TestName = "PrefixLexer.isdecimaldigit.01", ExpectedException=typeof<System.IndexOutOfRangeException>, ExpectedMessage = "Index was outside the bounds of the array.")>]
[<TestCase(1, TestName = "PrefixLexer.isdecimaldigit.02")>]
[<TestCase(2, TestName = "PrefixLexer.isdecimaldigit.03")>]
[<TestCase(3, TestName = "PrefixLexer.isdecimaldigit.04")>]
[<TestCase(4, TestName = "PrefixLexer.isdecimaldigit.05")>]
[<TestCase(5, TestName = "PrefixLexer.isdecimaldigit.06")>]
[<TestCase(6, TestName = "PrefixLexer.isdecimaldigit.07")>]
[<TestCase(7, TestName = "PrefixLexer.isdecimaldigit.08")>]
[<TestCase(8, TestName = "PrefixLexer.isdecimaldigit.09")>]
[<TestCase(9, TestName = "PrefixLexer.isdecimaldigit.010")>]
[<TestCase(10, TestName = "PrefixLexer.isdecimaldigit.011")>]
[<TestCase(11, TestName = "PrefixLexer.isdecimaldigit.012")>]
[<TestCase(12, TestName = "PrefixLexer.isdecimaldigit.013")>]
[<TestCase(13, TestName = "PrefixLexer.isdecimaldigit.014")>]
[<TestCase(14, TestName = "PrefixLexer.isdecimaldigit.015")>]
[<TestCase(15, TestName = "PrefixLexer.isdecimaldigit.016")>]
[<TestCase(16, TestName = "PrefixLexer.isdecimaldigit.017")>]
[<TestCase(17, TestName = "PrefixLexer.isdecimaldigit.018")>]
[<TestCase(18, TestName = "PrefixLexer.isdecimaldigit.019")>]
[<TestCase(19, TestName = "PrefixLexer.isdecimaldigit.020")>]
[<TestCase(20, TestName = "PrefixLexer.isdecimaldigit.021")>]
[<TestCase(21, TestName = "PrefixLexer.isdecimaldigit.022")>]
[<TestCase(22, TestName = "PrefixLexer.isdecimaldigit.023")>]
[<TestCase(23, TestName = "PrefixLexer.isdecimaldigit.024")>]
let ``function lex.isdecimaldigit`` idx =
    let (input, _) = isdecimaldigitValues.[idx]
    let (_, result) = isdecimaldigitValues.[idx]

    let functionResult = ArithmeticExpressionEvaluator.PrefixLexer.isdecimaldigit input
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
        // PrefixLexer.stringof.01
        // No input
        // throws noparse
        "",    // humans read this
        [],    // the stringof function reads this
        isdecimaldigit,
        ("",[])  // dummy value
    );
    (
        // idx 1
        // PrefixLexer.stringof.02
        // one number
        "1",
        ["1"],
        isdecimaldigit,
        ("1",[])
    );
    (
        // idx 2
        // PrefixLexer.stringof.03
        // two numbers
        "12",
        ["1";"2"],
        isdecimaldigit,
        ("12",[])
    );
    (
        // idx 3
        // PrefixLexer.stringof.04
        // three numbers
        "123",
        ["1";"2";"3"],
        isdecimaldigit,
        ("123",[])
    );
    |]
[<Test>]
[<TestCase(0, TestName = "PrefixLexer.stringof.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "PrefixLexer.stringof.02")>]
[<TestCase(2, TestName = "PrefixLexer.stringof.03")>]
[<TestCase(3, TestName = "PrefixLexer.stringof.04")>]
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
        // PrefixLexer.integer.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.PrefixLexer.integer function reads this
        (Integer "0",[])  // dummy value
    );
    (
        // idx 1
        // PrefixLexer.integer.02
        // One decimal digit
        "1",
        ["1"],
        (Integer "1",[])
    );
    (
        // idx 2
        // PrefixLexer.integer.03
        // Two decimal digits
        "10",
        ["1";"0"],
        (Integer "10",[])
    );
    (
        // idx 3
        // PrefixLexer.integer.04
        // Multiple decimal digits
        "1234567890",
        ["1";"2";"3";"4";"5";"6";"7";"8";"9";"0"],
        (Integer "1234567890",[])
    );
    (
        // idx 4
        // PrefixLexer.integer.05
        // a letter then a decimal digit
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a1",
        ["a";"1"],
        (Integer "0",[]) // dummy value
    );
    (
        // idx 5
        // PrefixLexer.integer.06
        // a decimal digit then a letter
        "1a",
        ["1";"a"],
        (Integer "1",["a"])
    );
    (
        // idx 6
        // PrefixLexer.integer.07
        // a decimal digit then an operator
        "1+",
        ["1";"+"],
        (Integer "1",["+"])
    );
    (
        // idx 7
        // PrefixLexer.integer.08
        // a decimal digit then a open paren
        "1(",
        ["1";"("],
        (Integer "1",["("])
    );
    (
        // idx 8
        // PrefixLexer.integer.09
        // a decimal digit then a close paren
        "1)",
        ["1";")"],
        (Integer "1",[")"])
    );
    (
        // idx 9
        // PrefixLexer.integer.010
        // a decimal digit then a space
        "1 ",
        ["1";" "],
        (Integer "1",[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "PrefixLexer.integer.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "PrefixLexer.integer.02")>]
[<TestCase(2, TestName = "PrefixLexer.integer.03")>]
[<TestCase(3, TestName = "PrefixLexer.integer.04")>]
[<TestCase(4, TestName = "PrefixLexer.integer.05", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(5, TestName = "PrefixLexer.integer.06")>]
[<TestCase(6, TestName = "PrefixLexer.integer.07")>]
[<TestCase(7, TestName = "PrefixLexer.integer.08")>]
[<TestCase(8, TestName = "PrefixLexer.integer.09")>]
[<TestCase(9, TestName = "PrefixLexer.integer.010")>]
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
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.PrefixLexer.integer
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
        // PrefixLexer.openParen.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.PrefixLexer.openParen function reads this
        (Comma,[])  // dummy value
    );
    (
        // idx 1
        // PrefixLexer.openParen.02
        // One open paren
        "(",
        ["("],
        (OpenParen,[])
    );
    (
        // idx 2
        // PrefixLexer.openParen.03
        // Two open parens
        "((",
        ["(";"("],
        (OpenParen,["("])
    );
    (
        // idx 3
        // PrefixLexer.openParen.04
        // a letter then a open paren
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a(",
        ["a";"("],
        (Comma,[]) // dummy value
    );
    (
        // idx 4
        // PrefixLexer.openParen.05
        // a open paren then a letter
        "(a",
        ["(";"a"],
        (OpenParen,["a"])
    );
    (
        // idx 5
        // PrefixLexer.openParen.06
        // a open paren then an operator
        "(+",
        ["(";"+"],
        (OpenParen,["+"])
    );
    (
        // idx 6
        // PrefixLexer.openParen.07
        // a open paren then a decimal digit
        "(1",
        ["(";"1"],
        (OpenParen,["1"])
    );
    (
        // idx 7
        // PrefixLexer.openParen.08
        // a open paren then a close paren
        "()",
        ["(";")"],
        (OpenParen, [")"])
    );
    (
        // idx 8
        // PrefixLexer.openParen.09
        // a open paren then a space
        "( ",
        ["(";" "],
        (OpenParen,[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "PrefixLexer.openParen.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "PrefixLexer.openParen.02")>]
[<TestCase(2, TestName = "PrefixLexer.openParen.03")>]
[<TestCase(3, TestName = "PrefixLexer.openParen.04", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(4, TestName = "PrefixLexer.openParen.05")>]
[<TestCase(5, TestName = "PrefixLexer.openParen.06")>]
[<TestCase(6, TestName = "PrefixLexer.openParen.07")>]
[<TestCase(7, TestName = "PrefixLexer.openParen.08")>]
[<TestCase(8, TestName = "PrefixLexer.openParen.09")>]
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
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.PrefixLexer.openParen
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
        // PrefixLexer.closeParen.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.PrefixLexer.closeParen function reads this
        (Comma,[])  // dummy value
    );
    (
        // idx 1
        // PrefixLexer.closeParen.02
        // One close paren
        ")",
        [")"],
        (CloseParen,[])
    );
    (
        // idx 2
        // PrefixLexer.closeParen.03
        // Two close parens
        "))",
        [")";")"],
        (CloseParen,[")"])
    );
    (
        // idx 3
        // PrefixLexer.closeParen.04
        // a letter then a close paren
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a)",
        ["a";")"],
        (Comma,[]) // dummy value
    );
    (
        // idx 4
        // PrefixLexer.closeParen.05
        // a close paren then a letter
        ")a",
        [")";"a"],
        (CloseParen,["a"])
    );
    (
        // idx 5
        // PrefixLexer.closeParen.06
        // a close paren then an operator
        ")+",
        [")";"+"],
        (CloseParen,["+"])
    );
    (
        // idx 6
        // PrefixLexer.closeParen.07
        // a close paren then a decimal digit
        ")1",
        [")";"1"],
        (CloseParen,["1"])
    );
    (
        // idx 7
        // PrefixLexer.closeParen.08
        // a close paren then a open paren
        ")(",
        [")";"("],
        (CloseParen, ["("])
    );
    (
        // idx 8
        // PrefixLexer.closeParen.09
        // a close paren then a space
        ") ",
        [")";" "],
        (CloseParen,[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "PrefixLexer.closeParen.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "PrefixLexer.closeParen.02")>]
[<TestCase(2, TestName = "PrefixLexer.closeParen.03")>]
[<TestCase(3, TestName = "PrefixLexer.closeParen.04", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(4, TestName = "PrefixLexer.closeParen.05")>]
[<TestCase(5, TestName = "PrefixLexer.closeParen.06")>]
[<TestCase(6, TestName = "PrefixLexer.closeParen.07")>]
[<TestCase(7, TestName = "PrefixLexer.closeParen.08")>]
[<TestCase(8, TestName = "PrefixLexer.closeParen.09")>]
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
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.PrefixLexer.closeParen
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
        // PrefixLexer.comma.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.PrefixLexer.comma function reads this
        (OpenParen,[])  // dummy value
    );
    (
        // idx 1
        // PrefixLexer.comma.02
        // One comma
        ",",
        [","],
        (Comma,[])
    );
    (
        // idx 2
        // PrefixLexer.comma.03
        // Two commas
        ",,",
        [",";","],
        (Comma,[","])
    );
    (
        // idx 3
        // PrefixLexer.comma.04
        // a letter then a comma
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a,",
        ["a";","],
        (OpenParen,[]) // dummy value
    );
    (
        // idx 4
        // PrefixLexer.comma.05
        // a comma then a letter
        ",a",
        [",";"a"],
        (Comma,["a"])
    );
    (
        // idx 5
        // PrefixLexer.comma.06
        // a comma then an operator
        ",+",
        [",";"+"],
        (Comma,["+"])
    );
    (
        // idx 6
        // PrefixLexer.comma.07
        // a comma then a decimal digit
        ",1",
        [",";"1"],
        (Comma,["1"])
    );
    (
        // idx 7
        // PrefixLexer.comma.08
        // a comma then a open paren
        ",(",
        [",";"("],
        (Comma, ["("])
    );
    (
        // idx 8
        // PrefixLexer.comma.09
        // a comma then a space
        ", ",
        [",";" "],
        (Comma,[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "PrefixLexer.comma.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "PrefixLexer.comma.02")>]
[<TestCase(2, TestName = "PrefixLexer.comma.03")>]
[<TestCase(3, TestName = "PrefixLexer.comma.04", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(4, TestName = "PrefixLexer.comma.05")>]
[<TestCase(5, TestName = "PrefixLexer.comma.06")>]
[<TestCase(6, TestName = "PrefixLexer.comma.07")>]
[<TestCase(7, TestName = "PrefixLexer.comma.08")>]
[<TestCase(8, TestName = "PrefixLexer.comma.09")>]
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
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.PrefixLexer.comma
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
        // PrefixLexer.operator.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.PrefixLexer.operator function reads this
        (OpenParen,[])  // dummy value
    );
    (
        // idx 1
        // PrefixLexer.operator.02
        // One star
        "*",
        ["*"],
        (Operator "*",[])
    );
    (
        // idx 2
        // PrefixLexer.operator.03
        // One plus
        "+",
        ["+"],
        (Operator "+",[])
    );
    (
        // idx 3
        // PrefixLexer.operator.04
        // One dash
        "-",
        ["-"],
        (Operator "-",[])
    );
    (
        // idx 4
        // PrefixLexer.operator.05
        // One slash
        "/",
        ["/"],
        (Operator "/",[])
    );
    (
        // idx 5
        // PrefixLexer.operator.06
        // Two stars
        "**",
        ["*";"*"],
        (Operator "*",["*"])
    );
    (
        // idx 6
        // PrefixLexer.operator.07
        // One start then one plus
        "*+",
        ["*";"+"],
        (Operator "*",["+"])
    );
    (
        // idx 7
        // PrefixLexer.operator.08
        // a letter then a star
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "a*",
        ["a";"*"],
        (OpenParen,[]) // dummy value
    );
    (
        // idx 8
        // PrefixLexer.operator.09
        // a star then a letter
        "*a",
        ["*";"a"],
        (Operator "*",["a"])
    );
    (
        // idx 9
        // PrefixLexer.operator.010
        // a star then a decimal digit
        "*1",
        ["*";"1"],
        (Operator "*",["1"])
    );
    (
        // idx 10
        // PrefixLexer.operator.011
        // a star then a open paren
        "*(",
        ["*";"("],
        (Operator "*", ["("])
    );
    (
        // idx 11
        // PrefixLexer.operator.012
        // a star then a close paren
        "*)",
        ["*";")"],
        (Operator "*", [")"])
    );
    (
        // idx 12
        // PrefixLexer.operator.013
        // a star then a space
        "* ",
        ["*";" "],
        (Operator "*",[" "])
    );
    |]

[<Test>]
[<TestCase(0, TestName = "PrefixLexer.operator.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "PrefixLexer.operator.02")>]
[<TestCase(2, TestName = "PrefixLexer.operator.03")>]
[<TestCase(3, TestName = "PrefixLexer.operator.04")>]
[<TestCase(4, TestName = "PrefixLexer.operator.05")>]
[<TestCase(5, TestName = "PrefixLexer.operator.06")>]
[<TestCase(6, TestName = "PrefixLexer.operator.07")>]
[<TestCase(7, TestName = "PrefixLexer.operator.08", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(8, TestName = "PrefixLexer.operator.09")>]
[<TestCase(9, TestName = "PrefixLexer.operator.010")>]
[<TestCase(10, TestName = "PrefixLexer.operator.011")>]
[<TestCase(11, TestName = "PrefixLexer.operator.012")>]
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
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.PrefixLexer.operator
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
        // PrefixLexer.rawtoken.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.PrefixLexer.rawtoken function reads this
        (Comma,[])  // dummy value
    );
    (
        // idx 1
        // PrefixLexer.rawtoken.02
        // One decimal digit
        "2",
        ["2"],
        (Integer "2",[])
    );
    (
        // idx 2
        // PrefixLexer.rawtoken.03
        // One open paren
        "(",
        ["("],
        (OpenParen ,[])
    );
    (
        // idx 3
        // PrefixLexer.rawtoken.04
        // One close paren
        ")",
        [")"],
        (CloseParen,[])
    );
    (
        // idx 4
        // PrefixLexer.rawtoken.05
        // ONe comma
        ",",
        [","],
        (Comma,[])
    );
    (
        // idx 5
        // PrefixLexer.rawtoken.06
        // One operator
        "*",
        ["*"],
        (Operator "*",[])
    );
    (
        // idx 6
        // PrefixLexer.rawtoken.07
        // One space
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " ",
        [" "],
        (Comma,[])  // dummy value
    );

    (
        // idx 7
        // PrefixLexer.rawtoken.08
        // Two decimal digits
        "23",
        ["2";"3"],
        (Integer "23",[])
    );
    (
        // idx 8
        // PrefixLexer.rawtoken.09
        // One decimal digit then one open paren
        "2(",
        ["2";"("],
        (Integer "2",["("])
    );
    (
        // idx 9
        // PrefixLexer.rawtoken.010
        // One decimal digit then one close paren
        "2)",
        ["2";")"],
        (Integer "2",[")"])
    );
    (
        // idx 10
        // PrefixLexer.rawtoken.011
        // One decimal digit then one comma
        "2,",
        ["2";","],
        (Integer "2",[","])
    );
    (
        // idx 11
        // PrefixLexer.rawtoken.012
        // One decimal digit then one operator
        "2*",
        ["2";"*"],
        (Integer "2",["*"])
    );
    (
        // idx 12
        // PrefixLexer.rawtoken.013
        // One decimal digit then one space
        "2 ",
        ["2";" "],
        (Integer "2",[" "])
    );

    (
        // idx 13
        // PrefixLexer.rawtoken.014
        // One open paren then one decimal digit
        "(2",
        ["(";"2"],
        (OpenParen,["2"])
    );
    (
        // idx 14
        // PrefixLexer.rawtoken.015
        // Two open parens
        "((",
        ["(";"("],
        (OpenParen,["("])
    );
    (
        // idx 15
        // PrefixLexer.rawtoken.016
        // One open paren then one close paren
        "()",
        ["(";")"],
        (OpenParen,[")"])
    );
    (
        // idx 16
        // PrefixLexer.rawtoken.017
        // One open paren then one comma
        "(,",
        ["(";","],
        (OpenParen,[","])
    );
    (
        // idx 17
        // PrefixLexer.rawtoken.018
        // One open paren then one operator
        "(*",
        ["(";"*"],
        (OpenParen,["*"])
    );
    (
        // idx 18
        // PrefixLexer.rawtoken.019
        // One open paren then one space
        "( ",
        ["(";" "],
        (OpenParen,[" "])
    );

    (
        // idx 19
        // PrefixLexer.rawtoken.020
        // One close paren then one decimal digit
        ")2",
        [")";"2"],
        (CloseParen,["2"])
    );
    (
        // idx 20
        // PrefixLexer.rawtoken.021
        // One close paren then one open paren
        ")(",
        [")";"("],
        (CloseParen,["("])
    );
    (
        // idx 21
        // PrefixLexer.rawtoken.022
        // Two close parens
        "))",
        [")";")"],
        (CloseParen,[")"])
    );
    (
        // idx 22
        // PrefixLexer.rawtoken.023
        // One close paren then one comma
        "),",
        [")";","],
        (CloseParen,[","])
    );
    (
        // idx 23
        // PrefixLexer.rawtoken.024
        // One close paren then one operator
        ")*",
        [")";"*"],
        (CloseParen,["*"])
    );
    (
        // idx 24
        // PrefixLexer.rawtoken.025
        // One close paren then one space
        ") ",
        [")";" "],
        (CloseParen,[" "])
    );

    (
        // idx 25
        // PrefixLexer.rawtoken.026
        // One comma then one decimal digit
        ",2",
        [",";"2"],
        (Comma,["2"])
    );
    (
        // idx 26
        // PrefixLexer.rawtoken.027
        // One comma then one comma
        ",(",
        [",";"("],
        (Comma,["("])
    );
    (
        // idx 27
        // PrefixLexer.rawtoken.028
        // One comma then one close paren
        ",)",
        [",";")"],
        (Comma,[")"])
    );
    (
        // idx 28
        // PrefixLexer.rawtoken.029
        // Two commas
        ",,",
        [",";","],
        (Comma,[","])
    );
    (
        // idx 29
        // PrefixLexer.rawtoken.030
        // One comma then one operator
        ",*",
        [",";"*"],
        (Comma,["*"])
    );
    (
        // idx 30
        // PrefixLexer.rawtoken.031
        // One comma then one space
        ", ",
        [",";" "],
        (Comma,[" "])
    );

    (
        // idx 31
        // PrefixLexer.rawtoken.032
        // One operator then one decimal digit
        "*2",
        ["*";"2"],
        (Operator "*",["2"])
    );
    (
        // idx 32
        // PrefixLexer.rawtoken.033
        // One operator then one operator
        "*(",
        ["*";"("],
        (Operator "*",["("])
    );
    (
        // idx 33
        // PrefixLexer.rawtoken.034
        // One operator then one close paren
        "*)",
        ["*";")"],
        (Operator "*",[")"])
    );
    (
        // idx 34
        // PrefixLexer.rawtoken.035
        // One operator then one comma
        "*,",
        ["*";","],
        (Operator "*",[","])
    );
    (
        // idx 35
        // PrefixLexer.rawtoken.036
        // Two operators
        "**",
        ["*";"*"],
        (Operator "*",["*"])
    );
    (
        // idx 36
        // PrefixLexer.rawtoken.037
        // One operator then one space
        "* ",
        ["*";" "],
        (Operator "*",[" "])
    );

    (
        // idx 37
        // PrefixLexer.rawtoken.038
        // One whitespace then one decimal digit
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " 2",
        [" ";"2"],
        (Comma,[])  // dummy value
    );
    (
        // idx 38
        // PrefixLexer.rawtoken.039
        // One whitespace then one operator
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " (",
        [" ";"("],
        (Comma,[])  // dummy value
    );
    (
        // idx 39
        // PrefixLexer.rawtoken.040
        // One whitespace then one close paren
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " )",
        [" ";")"],
        (Comma,[])  // dummy value
    );
    (
        // idx 40
        // PrefixLexer.rawtoken.041
        // One whitespace then one comma
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " ,",
        [" ";","],
        (Comma,[])  // dummy value
    );
    (
        // idx 41
        // PrefixLexer.rawtoken.042
        // One whitespace then one operator
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        " *",
        [" ";"*"],
        (Comma,[])  // dummy value
    );
    (
        // idx 42
        // PrefixLexer.rawtoken.043
        // Two whitewhitespace
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "  ",
        [" ";" "],
        (Comma,[])  // dummy value
    );


    |]

[<Test>]
[<TestCase(0, TestName = "PrefixLexer.rawtoken.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "PrefixLexer.rawtoken.02")>]
[<TestCase(2, TestName = "PrefixLexer.rawtoken.03")>]
[<TestCase(3, TestName = "PrefixLexer.rawtoken.04")>]
[<TestCase(4, TestName = "PrefixLexer.rawtoken.05")>]
[<TestCase(5, TestName = "PrefixLexer.rawtoken.06")>]
[<TestCase(6, TestName = "PrefixLexer.rawtoken.07", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(7, TestName = "PrefixLexer.rawtoken.08")>]
[<TestCase(8, TestName = "PrefixLexer.rawtoken.09")>]
[<TestCase(9, TestName = "PrefixLexer.rawtoken.010")>]
[<TestCase(10, TestName = "PrefixLexer.rawtoken.011")>]
[<TestCase(11, TestName = "PrefixLexer.rawtoken.012")>]
[<TestCase(12, TestName = "PrefixLexer.rawtoken.013")>]
[<TestCase(13, TestName = "PrefixLexer.rawtoken.014")>]
[<TestCase(14, TestName = "PrefixLexer.rawtoken.015")>]
[<TestCase(15, TestName = "PrefixLexer.rawtoken.016")>]
[<TestCase(16, TestName = "PrefixLexer.rawtoken.017")>]
[<TestCase(17, TestName = "PrefixLexer.rawtoken.018")>]
[<TestCase(18, TestName = "PrefixLexer.rawtoken.019")>]
[<TestCase(19, TestName = "PrefixLexer.rawtoken.020")>]
[<TestCase(20, TestName = "PrefixLexer.rawtoken.021")>]
[<TestCase(21, TestName = "PrefixLexer.rawtoken.022")>]
[<TestCase(22, TestName = "PrefixLexer.rawtoken.023")>]
[<TestCase(23, TestName = "PrefixLexer.rawtoken.024")>]
[<TestCase(24, TestName = "PrefixLexer.rawtoken.025")>]
[<TestCase(25, TestName = "PrefixLexer.rawtoken.026")>]
[<TestCase(26, TestName = "PrefixLexer.rawtoken.027")>]
[<TestCase(27, TestName = "PrefixLexer.rawtoken.028")>]
[<TestCase(28, TestName = "PrefixLexer.rawtoken.029")>]
[<TestCase(29, TestName = "PrefixLexer.rawtoken.030")>]
[<TestCase(30, TestName = "PrefixLexer.rawtoken.031")>]
[<TestCase(31, TestName = "PrefixLexer.rawtoken.032")>]
[<TestCase(32, TestName = "PrefixLexer.rawtoken.033")>]
[<TestCase(33, TestName = "PrefixLexer.rawtoken.034")>]
[<TestCase(34, TestName = "PrefixLexer.rawtoken.035")>]
[<TestCase(35, TestName = "PrefixLexer.rawtoken.036")>]
[<TestCase(36, TestName = "PrefixLexer.rawtoken.037")>]
[<TestCase(37, TestName = "PrefixLexer.rawtoken.038", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(38, TestName = "PrefixLexer.rawtoken.039", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(39, TestName = "PrefixLexer.rawtoken.040", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(40, TestName = "PrefixLexer.rawtoken.041", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(41, TestName = "PrefixLexer.rawtoken.042", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(42, TestName = "PrefixLexer.rawtoken.043", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
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
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.PrefixLexer.rawtoken
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
        // PrefixLexer.spacedtoken.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.PrefixLexer.spacedtoken function reads this
        (Comma,[])  // dummy value
    );
    (
        // idx 1
        // PrefixLexer.spacedtoken.02
        // One decimal digit
        "2",
        ["2"],
        (Integer "2",[])
    );
    (
        // idx 2
        // PrefixLexer.spacedtoken.03
        // One open paren
        "(",
        ["("],
        (OpenParen ,[])
    );
    (
        // idx 3
        // PrefixLexer.spacedtoken.04
        // One close paren
        ")",
        [")"],
        (CloseParen,[])
    );
    (
        // idx 4
        // PrefixLexer.spacedtoken.05
        // ONe comma
        ",",
        [","],
        (Comma,[])
    );
    (
        // idx 5
        // PrefixLexer.spacedtoken.06
        // One operator
        "*",
        ["*"],
        (Operator "*",[])
    );
    (
        // idx 6
        // PrefixLexer.spacedtoken.07
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
        // PrefixLexer.spacedtoken.08
        // Two decimal digits
        "23",
        ["2";"3"],
        (Integer "23",[])
    );
    (
        // idx 8
        // PrefixLexer.spacedtoken.09
        // One decimal digit then one open paren
        "2(",
        ["2";"("],
        (Integer "2",["("])
    );
    (
        // idx 9
        // PrefixLexer.spacedtoken.010
        // One decimal digit then one close paren
        "2)",
        ["2";")"],
        (Integer "2",[")"])
    );
    (
        // idx 10
        // PrefixLexer.spacedtoken.011
        // One decimal digit then one comma
        "2,",
        ["2";","],
        (Integer "2",[","])
    );
    (
        // idx 11
        // PrefixLexer.spacedtoken.012
        // One decimal digit then one operator
        "2*",
        ["2";"*"],
        (Integer "2",["*"])
    );
    (
        // idx 12
        // PrefixLexer.spacedtoken.013
        // One decimal digit then one space
        "2 ",
        ["2";" "],
        (Integer "2",[" "])
    );

    (
        // idx 13
        // PrefixLexer.spacedtoken.014
        // One open paren then one decimal digit
        "(2",
        ["(";"2"],
        (OpenParen,["2"])
    );
    (
        // idx 14
        // PrefixLexer.spacedtoken.015
        // Two open parens
        "((",
        ["(";"("],
        (OpenParen,["("])
    );
    (
        // idx 15
        // PrefixLexer.spacedtoken.016
        // One open paren then one close paren
        "()",
        ["(";")"],
        (OpenParen,[")"])
    );
    (
        // idx 16
        // PrefixLexer.spacedtoken.017
        // One open paren then one comma
        "(,",
        ["(";","],
        (OpenParen,[","])
    );
    (
        // idx 17
        // PrefixLexer.spacedtoken.018
        // One open paren then one operator
        "(*",
        ["(";"*"],
        (OpenParen,["*"])
    );
    (
        // idx 18
        // PrefixLexer.spacedtoken.019
        // One open paren then one space
        "( ",
        ["(";" "],
        (OpenParen,[" "])
    );

    (
        // idx 19
        // PrefixLexer.spacedtoken.020
        // One close paren then one decimal digit
        ")2",
        [")";"2"],
        (CloseParen,["2"])
    );
    (
        // idx 20
        // PrefixLexer.spacedtoken.021
        // One close paren then one open paren
        ")(",
        [")";"("],
        (CloseParen,["("])
    );
    (
        // idx 21
        // PrefixLexer.spacedtoken.022
        // Two close parens
        "))",
        [")";")"],
        (CloseParen,[")"])
    );
    (
        // idx 22
        // PrefixLexer.spacedtoken.023
        // One close paren then one comma
        "),",
        [")";","],
        (CloseParen,[","])
    );
    (
        // idx 23
        // PrefixLexer.spacedtoken.024
        // One close paren then one operator
        ")*",
        [")";"*"],
        (CloseParen,["*"])
    );
    (
        // idx 24
        // PrefixLexer.spacedtoken.025
        // One close paren then one space
        ") ",
        [")";" "],
        (CloseParen,[" "])
    );

    (
        // idx 25
        // PrefixLexer.spacedtoken.026
        // One comma then one decimal digit
        ",2",
        [",";"2"],
        (Comma,["2"])
    );
    (
        // idx 26
        // PrefixLexer.spacedtoken.027
        // One comma then one comma
        ",(",
        [",";"("],
        (Comma,["("])
    );
    (
        // idx 27
        // PrefixLexer.spacedtoken.028
        // One comma then one close paren
        ",)",
        [",";")"],
        (Comma,[")"])
    );
    (
        // idx 28
        // PrefixLexer.spacedtoken.029
        // Two commas
        ",,",
        [",";","],
        (Comma,[","])
    );
    (
        // idx 29
        // PrefixLexer.spacedtoken.030
        // One comma then one operator
        ",*",
        [",";"*"],
        (Comma,["*"])
    );
    (
        // idx 30
        // PrefixLexer.spacedtoken.031
        // One comma then one space
        ", ",
        [",";" "],
        (Comma,[" "])
    );

    (
        // idx 31
        // PrefixLexer.spacedtoken.032
        // One operator then one decimal digit
        "*2",
        ["*";"2"],
        (Operator "*",["2"])
    );
    (
        // idx 32
        // PrefixLexer.spacedtoken.033
        // One operator then one operator
        "*(",
        ["*";"("],
        (Operator "*",["("])
    );
    (
        // idx 33
        // PrefixLexer.spacedtoken.034
        // One operator then one close paren
        "*)",
        ["*";")"],
        (Operator "*",[")"])
    );
    (
        // idx 34
        // PrefixLexer.spacedtoken.035
        // One operator then one comma
        "*,",
        ["*";","],
        (Operator "*",[","])
    );
    (
        // idx 35
        // PrefixLexer.spacedtoken.036
        // Two operators
        "**",
        ["*";"*"],
        (Operator "*",["*"])
    );
    (
        // idx 36
        // PrefixLexer.spacedtoken.037
        // One operator then one space
        "* ",
        ["*";" "],
        (Operator "*",[" "])
    );

    (
        // idx 37
        // PrefixLexer.spacedtoken.038
        // One whitespace then one decimal digit
        " 2",
        [" ";"2"],
        (Integer "2",[])
    );
    (
        // idx 38
        // PrefixLexer.spacedtoken.039
        // One whitespace then one operator
        " (",
        [" ";"("],
        (OpenParen,[])
    );
    (
        // idx 39
        // PrefixLexer.spacedtoken.040
        // One whitespace then one close paren
        " )",
        [" ";")"],
        (CloseParen,[])
    );
    (
        // idx 40
        // PrefixLexer.spacedtoken.041
        // One whitespace then one comma
        " ,",
        [" ";","],
        (Comma,[])
    );
    (
        // idx 41
        // PrefixLexer.spacedtoken.042
        // One whitespace then one operator
        " *",
        [" ";"*"],
        (Operator "*",[])
    );
    (
        // idx 42
        // PrefixLexer.spacedtoken.043
        // Two whitewhitespace
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        "  ",
        [" ";" "],
        (Comma,[])  // dummy value
    );


    |]

[<Test>]
[<TestCase(0, TestName = "PrefixLexer.spacedtoken.01", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(1, TestName = "PrefixLexer.spacedtoken.02")>]
[<TestCase(2, TestName = "PrefixLexer.spacedtoken.03")>]
[<TestCase(3, TestName = "PrefixLexer.spacedtoken.04")>]
[<TestCase(4, TestName = "PrefixLexer.spacedtoken.05")>]
[<TestCase(5, TestName = "PrefixLexer.spacedtoken.06")>]
[<TestCase(6, TestName = "PrefixLexer.spacedtoken.07", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
[<TestCase(7, TestName = "PrefixLexer.spacedtoken.08")>]
[<TestCase(8, TestName = "PrefixLexer.spacedtoken.09")>]
[<TestCase(9, TestName = "PrefixLexer.spacedtoken.010")>]
[<TestCase(10, TestName = "PrefixLexer.spacedtoken.011")>]
[<TestCase(11, TestName = "PrefixLexer.spacedtoken.012")>]
[<TestCase(12, TestName = "PrefixLexer.spacedtoken.013")>]
[<TestCase(13, TestName = "PrefixLexer.spacedtoken.014")>]
[<TestCase(14, TestName = "PrefixLexer.spacedtoken.015")>]
[<TestCase(15, TestName = "PrefixLexer.spacedtoken.016")>]
[<TestCase(16, TestName = "PrefixLexer.spacedtoken.017")>]
[<TestCase(17, TestName = "PrefixLexer.spacedtoken.018")>]
[<TestCase(18, TestName = "PrefixLexer.spacedtoken.019")>]
[<TestCase(19, TestName = "PrefixLexer.spacedtoken.020")>]
[<TestCase(20, TestName = "PrefixLexer.spacedtoken.021")>]
[<TestCase(21, TestName = "PrefixLexer.spacedtoken.022")>]
[<TestCase(22, TestName = "PrefixLexer.spacedtoken.023")>]
[<TestCase(23, TestName = "PrefixLexer.spacedtoken.024")>]
[<TestCase(24, TestName = "PrefixLexer.spacedtoken.025")>]
[<TestCase(25, TestName = "PrefixLexer.spacedtoken.026")>]
[<TestCase(26, TestName = "PrefixLexer.spacedtoken.027")>]
[<TestCase(27, TestName = "PrefixLexer.spacedtoken.028")>]
[<TestCase(28, TestName = "PrefixLexer.spacedtoken.029")>]
[<TestCase(29, TestName = "PrefixLexer.spacedtoken.030")>]
[<TestCase(30, TestName = "PrefixLexer.spacedtoken.031")>]
[<TestCase(31, TestName = "PrefixLexer.spacedtoken.032")>]
[<TestCase(32, TestName = "PrefixLexer.spacedtoken.033")>]
[<TestCase(33, TestName = "PrefixLexer.spacedtoken.034")>]
[<TestCase(34, TestName = "PrefixLexer.spacedtoken.035")>]
[<TestCase(35, TestName = "PrefixLexer.spacedtoken.036")>]
[<TestCase(36, TestName = "PrefixLexer.spacedtoken.037")>]
[<TestCase(37, TestName = "PrefixLexer.spacedtoken.038")>]
[<TestCase(38, TestName = "PrefixLexer.spacedtoken.039")>]
[<TestCase(39, TestName = "PrefixLexer.spacedtoken.040")>]
[<TestCase(40, TestName = "PrefixLexer.spacedtoken.041")>]
[<TestCase(41, TestName = "PrefixLexer.spacedtoken.042")>]
[<TestCase(42, TestName = "PrefixLexer.spacedtoken.043", ExpectedException=typeof<ArithmeticExpressionEvaluator.ParserCombinator.Noparse>, ExpectedMessage = "Exception of type 'ArithmeticExpressionEvaluator.ParserCombinator+Noparse' was thrown.")>]
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
    let stringParser : string list -> token * string list = ArithmeticExpressionEvaluator.PrefixLexer.spacedtoken
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
        // PrefixLexer.tokens.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.PrefixLexer.tokens function reads this
        ([],[])  // Notice no exception for empty list.
    );
    (
        // idx 1
        // PrefixLexer.tokens.02
        // One decimal digit
        "2",
        ["2"],
        ([Integer "2"],[])
    );
    (
        // idx 2
        // PrefixLexer.tokens.03
        // One open paren
        "(",
        ["("],
        ([OpenParen],[])
    );
    (
        // idx 3
        // PrefixLexer.tokens.04
        // One close paren
        ")",
        [")"],
        ([CloseParen],[])
    );
    (
        // idx 4
        // PrefixLexer.tokens.05
        // ONe comma
        ",",
        [","],
        ([Comma],[])
    );
    (
        // idx 5
        // PrefixLexer.tokens.06
        // One operator
        "*",
        ["*"],
        ([Operator "*"],[])
    );
    (
        // idx 6
        // PrefixLexer.tokens.07
        // One space
        " ",
        [" "],
        ([],[" "])  // Notice no exception for a single space.
    );

    (
        // idx 7
        // PrefixLexer.tokens.08
        // Two decimal digits
        "23",
        ["2";"3"],
        ([Integer "23"],[])
    );
    (
        // idx 8
        // PrefixLexer.tokens.09
        // One decimal digit then one open paren
        "2(",
        ["2";"("],
        ([Integer "2"; OpenParen],[])
    );
    (
        // idx 9
        // PrefixLexer.tokens.010
        // One decimal digit then one close paren
        "2)",
        ["2";")"],
        ([Integer "2"; CloseParen],[])
    );
    (
        // idx 10
        // PrefixLexer.tokens.011
        // One decimal digit then one comma
        "2,",
        ["2";","],
        ([Integer "2"; Comma],[])
    );
    (
        // idx 11
        // PrefixLexer.tokens.012
        // One decimal digit then one operator
        "2*",
        ["2";"*"],
        ([Integer "2"; Operator "*"],[])
    );
    (
        // idx 12
        // PrefixLexer.tokens.013
        // One decimal digit then one space
        "2 ",
        ["2";" "],
        ([Integer "2"],[" "])
    );

    (
        // idx 13
        // PrefixLexer.tokens.014
        // One open paren then one decimal digit
        "(2",
        ["(";"2"],
        ([OpenParen; Integer "2"],[])
    );
    (
        // idx 14
        // PrefixLexer.tokens.015
        // Two open parens
        "((",
        ["(";"("],
        ([OpenParen; OpenParen],[])
    );
    (
        // idx 15
        // PrefixLexer.tokens.016
        // One open paren then one close paren
        "()",
        ["(";")"],
        ([OpenParen; CloseParen],[])
    );
    (
        // idx 16
        // PrefixLexer.tokens.017
        // One open paren then one comma
        "(,",
        ["(";","],
        ([OpenParen; Comma],[])
    );
    (
        // idx 17
        // PrefixLexer.tokens.018
        // One open paren then one operator
        "(*",
        ["(";"*"],
        ([OpenParen; Operator "*"],[])
    );
    (
        // idx 18
        // PrefixLexer.tokens.019
        // One open paren then one space
        "( ",
        ["(";" "],
        ([OpenParen],[" "])
    );

    (
        // idx 19
        // PrefixLexer.tokens.020
        // One close paren then one decimal digit
        ")2",
        [")";"2"],
        ([CloseParen; Integer "2"],[])
    );
    (
        // idx 20
        // PrefixLexer.tokens.021
        // One close paren then one open paren
        ")(",
        [")";"("],
        ([CloseParen; OpenParen],[])
    );
    (
        // idx 21
        // PrefixLexer.tokens.022
        // Two close parens
        "))",
        [")";")"],
        ([CloseParen; CloseParen],[])
    );
    (
        // idx 22
        // PrefixLexer.tokens.023
        // One close paren then one comma
        "),",
        [")";","],
        ([CloseParen; Comma],[])
    );
    (
        // idx 23
        // PrefixLexer.tokens.024
        // One close paren then one operator
        ")*",
        [")";"*"],
        ([CloseParen; Operator "*"],[])
    );
    (
        // idx 24
        // PrefixLexer.tokens.025
        // One close paren then one space
        ") ",
        [")";" "],
        ([CloseParen],[" "])
    );

    (
        // idx 25
        // PrefixLexer.tokens.026
        // One comma then one decimal digit
        ",2",
        [",";"2"],
        ([Comma; Integer "2"],[])
    );
    (
        // idx 26
        // PrefixLexer.tokens.027
        // One comma then one comma
        ",(",
        [",";"("],
        ([Comma; OpenParen],[])
    );
    (
        // idx 27
        // PrefixLexer.tokens.028
        // One comma then one close paren
        ",)",
        [",";")"],
        ([Comma; CloseParen],[])
    );
    (
        // idx 28
        // PrefixLexer.tokens.029
        // Two commas
        ",,",
        [",";","],
        ([Comma; Comma],[])
    );
    (
        // idx 29
        // PrefixLexer.tokens.030
        // One comma then one operator
        ",*",
        [",";"*"],
        ([Comma; Operator "*"],[])
    );
    (
        // idx 30
        // PrefixLexer.tokens.031
        // One comma then one space
        ", ",
        [",";" "],
        ([Comma],[" "])
    );

    (
        // idx 31
        // PrefixLexer.tokens.032
        // One operator then one decimal digit
        "*2",
        ["*";"2"],
        ([Operator "*"; Integer "2"],[])
    );
    (
        // idx 32
        // PrefixLexer.tokens.033
        // One operator then one operator
        "*(",
        ["*";"("],
        ([Operator "*"; OpenParen],[])
    );
    (
        // idx 33
        // PrefixLexer.tokens.034
        // One operator then one close paren
        "*)",
        ["*";")"],
        ([Operator "*"; CloseParen],[])
    );
    (
        // idx 34
        // PrefixLexer.tokens.035
        // One operator then one comma
        "*,",
        ["*";","],
        ([Operator "*"; Comma],[])
    );
    (
        // idx 35
        // PrefixLexer.tokens.036
        // Two operators
        "**",
        ["*";"*"],
        ([Operator "*"; Operator "*"],[])
    );
    (
        // idx 36
        // PrefixLexer.tokens.037
        // One operator then one space
        "* ",
        ["*";" "],
        ([Operator "*"],[" "])
    );

    (
        // idx 37
        // PrefixLexer.tokens.038
        // One whitespace then one decimal digit
        " 2",
        [" ";"2"],
        ([Integer "2"],[])
    );
    (
        // idx 38
        // PrefixLexer.tokens.039
        // One whitespace then one operator
        " (",
        [" ";"("],
        ([OpenParen],[])
    );
    (
        // idx 39
        // PrefixLexer.tokens.040
        // One whitespace then one close paren
        " )",
        [" ";")"],
        ([CloseParen],[])
    );
    (
        // idx 40
        // PrefixLexer.tokens.041
        // One whitespace then one comma
        " ,",
        [" ";","],
        ([Comma],[])
    );
    (
        // idx 41
        // PrefixLexer.tokens.042
        // One whitespace then one operator
        " *",
        [" ";"*"],
        ([Operator "*"],[])
    );
    (
        // idx 42
        // PrefixLexer.tokens.043
        // Two whitewhitespace
        "  ",
        [" ";" "],
        ([],[" "; " "])
    );
    (
        // idx 43
        // PrefixLexer.tokens.044
        // One alpha character
        "a",
        ["a"],
        ([],["a"])
    );
    (
        // idx 44
        // PrefixLexer.tokens.045
        // One alpha character
        "1 ",
        ["1"; " "],
        ([Integer "1"],[" "])  // Notice: tokens function does not process trailing whitespace.
    );
    |]

[<Test>]
[<TestCase(0, TestName = "PrefixLexer.tokens.01")>]
[<TestCase(1, TestName = "PrefixLexer.tokens.02")>]
[<TestCase(2, TestName = "PrefixLexer.tokens.03")>]
[<TestCase(3, TestName = "PrefixLexer.tokens.04")>]
[<TestCase(4, TestName = "PrefixLexer.tokens.05")>]
[<TestCase(5, TestName = "PrefixLexer.tokens.06")>]
[<TestCase(6, TestName = "PrefixLexer.tokens.07")>]
[<TestCase(7, TestName = "PrefixLexer.tokens.08")>]
[<TestCase(8, TestName = "PrefixLexer.tokens.09")>]
[<TestCase(9, TestName = "PrefixLexer.tokens.010")>]
[<TestCase(10, TestName = "PrefixLexer.tokens.011")>]
[<TestCase(11, TestName = "PrefixLexer.tokens.012")>]
[<TestCase(12, TestName = "PrefixLexer.tokens.013")>]
[<TestCase(13, TestName = "PrefixLexer.tokens.014")>]
[<TestCase(14, TestName = "PrefixLexer.tokens.015")>]
[<TestCase(15, TestName = "PrefixLexer.tokens.016")>]
[<TestCase(16, TestName = "PrefixLexer.tokens.017")>]
[<TestCase(17, TestName = "PrefixLexer.tokens.018")>]
[<TestCase(18, TestName = "PrefixLexer.tokens.019")>]
[<TestCase(19, TestName = "PrefixLexer.tokens.020")>]
[<TestCase(20, TestName = "PrefixLexer.tokens.021")>]
[<TestCase(21, TestName = "PrefixLexer.tokens.022")>]
[<TestCase(22, TestName = "PrefixLexer.tokens.023")>]
[<TestCase(23, TestName = "PrefixLexer.tokens.024")>]
[<TestCase(24, TestName = "PrefixLexer.tokens.025")>]
[<TestCase(25, TestName = "PrefixLexer.tokens.026")>]
[<TestCase(26, TestName = "PrefixLexer.tokens.027")>]
[<TestCase(27, TestName = "PrefixLexer.tokens.028")>]
[<TestCase(28, TestName = "PrefixLexer.tokens.029")>]
[<TestCase(29, TestName = "PrefixLexer.tokens.030")>]
[<TestCase(30, TestName = "PrefixLexer.tokens.031")>]
[<TestCase(31, TestName = "PrefixLexer.tokens.032")>]
[<TestCase(32, TestName = "PrefixLexer.tokens.033")>]
[<TestCase(33, TestName = "PrefixLexer.tokens.034")>]
[<TestCase(34, TestName = "PrefixLexer.tokens.035")>]
[<TestCase(35, TestName = "PrefixLexer.tokens.036")>]
[<TestCase(36, TestName = "PrefixLexer.tokens.037")>]
[<TestCase(37, TestName = "PrefixLexer.tokens.038")>]
[<TestCase(38, TestName = "PrefixLexer.tokens.039")>]
[<TestCase(39, TestName = "PrefixLexer.tokens.040")>]
[<TestCase(40, TestName = "PrefixLexer.tokens.041")>]
[<TestCase(41, TestName = "PrefixLexer.tokens.042")>]
[<TestCase(42, TestName = "PrefixLexer.tokens.043")>]
[<TestCase(43, TestName = "PrefixLexer.tokens.044")>]
[<TestCase(44, TestName = "PrefixLexer.tokens.045")>]
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
    let stringParser : string list -> token list * string list = ArithmeticExpressionEvaluator.PrefixLexer.tokens
    let (current, rest) = stringParser internalForm
//    printfn "expected result: %A %A" currentResult restResult
//    printfn "function result: %A %A" current rest
    Assert.AreEqual(current, currentResult)
    Assert.AreEqual(rest, restResult)

//#endregion

//#region "prefixLex tests"

// The first string is what humans expect to read
// and the second string list is what the function reads.
// Both are shown to make the test easier to comprehend.
let private prefixLexStringTypeValues : (string * string list * token list)[] = [|
    (
        // idx 0
        // PrefixLexer.prefixLex.01
        // throws ArithmeticExpressionEvaluator.ParserCombinator.Noparse
        // No input
        "",    // humans read this
        [],    // the ArithmeticExpressionEvaluator.PrefixLexer.lex function reads this
        []  // Notice no exception for empty list.
    );
    (
        // idx 1
        // PrefixLexer.prefixLex.02
        // One decimal digit
        "2",
        ["2"],
        [Integer "2"]
    );
    (
        // idx 2
        // PrefixLexer.prefixLex.03
        // One open paren
        "(",
        ["("],
        [OpenParen]
    );
    (
        // idx 3
        // PrefixLexer.prefixLex.04
        // One close paren
        ")",
        [")"],
        [CloseParen]
    );
    (
        // idx 4
        // PrefixLexer.prefixLex.05
        // ONe comma
        ",",
        [","],
        [Comma]
    );
    (
        // idx 5
        // PrefixLexer.prefixLex.06
        // One operator
        "*",
        ["*"],
        [Operator "*"]
    );
    (
        // idx 6
        // PrefixLexer.prefixLex.07
        // One space
        " ",
        [" "],
        []  // Notice no exception for a single space.
    );

    (
        // idx 7
        // PrefixLexer.prefixLex.08
        // Two decimal digits
        "23",
        ["2";"3"],
        [Integer "23"]
    );
    (
        // idx 8
        // PrefixLexer.prefixLex.09
        // One decimal digit then one open paren
        "2(",
        ["2";"("],
        [Integer "2"; OpenParen]
    );
    (
        // idx 9
        // PrefixLexer.prefixLex.010
        // One decimal digit then one close paren
        "2)",
        ["2";")"],
        [Integer "2"; CloseParen]
    );
    (
        // idx 10
        // PrefixLexer.prefixLex.011
        // One decimal digit then one comma
        "2,",
        ["2";","],
        [Integer "2"; Comma]
    );
    (
        // idx 11
        // PrefixLexer.prefixLex.012
        // One decimal digit then one operator
        "2*",
        ["2";"*"],
        [Integer "2"; Operator "*"]
    );
    (
        // idx 12
        // PrefixLexer.prefixLex.013
        // One decimal digit then one space
        "2 ",
        ["2";" "],
        [Integer "2"]
    );

    (
        // idx 13
        // PrefixLexer.prefixLex.014
        // One open paren then one decimal digit
        "(2",
        ["(";"2"],
        [OpenParen; Integer "2"]
    );
    (
        // idx 14
        // PrefixLexer.prefixLex.015
        // Two open parens
        "((",
        ["(";"("],
        [OpenParen; OpenParen]
    );
    (
        // idx 15
        // PrefixLexer.prefixLex.016
        // One open paren then one close paren
        "()",
        ["(";")"],
        [OpenParen; CloseParen]
    );
    (
        // idx 16
        // PrefixLexer.prefixLex.017
        // One open paren then one comma
        "(,",
        ["(";","],
        [OpenParen; Comma]
    );
    (
        // idx 17
        // PrefixLexer.prefixLex.018
        // One open paren then one operator
        "(*",
        ["(";"*"],
        [OpenParen; Operator "*"]
    );
    (
        // idx 18
        // PrefixLexer.prefixLex.019
        // One open paren then one space
        "( ",
        ["(";" "],
        [OpenParen]
    );

    (
        // idx 19
        // PrefixLexer.prefixLex.020
        // One close paren then one decimal digit
        ")2",
        [")";"2"],
        [CloseParen; Integer "2"]
    );
    (
        // idx 20
        // PrefixLexer.prefixLex.021
        // One close paren then one open paren
        ")(",
        [")";"("],
        [CloseParen; OpenParen]
    );
    (
        // idx 21
        // PrefixLexer.prefixLex.022
        // Two close parens
        "))",
        [")";")"],
        [CloseParen; CloseParen]
    );
    (
        // idx 22
        // PrefixLexer.prefixLex.023
        // One close paren then one comma
        "),",
        [")";","],
        [CloseParen; Comma]
    );
    (
        // idx 23
        // PrefixLexer.prefixLex.024
        // One close paren then one operator
        ")*",
        [")";"*"],
        [CloseParen; Operator "*"]
    );
    (
        // idx 24
        // PrefixLexer.prefixLex.025
        // One close paren then one space
        ") ",
        [")";" "],
        [CloseParen]
    );

    (
        // idx 25
        // PrefixLexer.prefixLex.026
        // One comma then one decimal digit
        ",2",
        [",";"2"],
        [Comma; Integer "2"]
    );
    (
        // idx 26
        // PrefixLexer.prefixLex.027
        // One comma then one comma
        ",(",
        [",";"("],
        [Comma; OpenParen]
    );
    (
        // idx 27
        // PrefixLexer.prefixLex.028
        // One comma then one close paren
        ",)",
        [",";")"],
        [Comma; CloseParen]
    );
    (
        // idx 28
        // PrefixLexer.prefixLex.029
        // Two commas
        ",,",
        [",";","],
        [Comma; Comma]
    );
    (
        // idx 29
        // PrefixLexer.prefixLex.030
        // One comma then one operator
        ",*",
        [",";"*"],
        [Comma; Operator "*"]
    );
    (
        // idx 30
        // PrefixLexer.prefixLex.031
        // One comma then one space
        ", ",
        [",";" "],
        [Comma]
    );

    (
        // idx 31
        // PrefixLexer.prefixLex.032
        // One operator then one decimal digit
        "*2",
        ["*";"2"],
        [Operator "*"; Integer "2"]
    );
    (
        // idx 32
        // PrefixLexer.prefixLex.033
        // One operator then one operator
        "*(",
        ["*";"("],
        [Operator "*"; OpenParen]
    );
    (
        // idx 33
        // PrefixLexer.prefixLex.034
        // One operator then one close paren
        "*)",
        ["*";")"],
        [Operator "*"; CloseParen]
    );
    (
        // idx 34
        // PrefixLexer.prefixLex.035
        // One operator then one comma
        "*,",
        ["*";","],
        [Operator "*"; Comma]
    );
    (
        // idx 35
        // PrefixLexer.prefixLex.036
        // Two operators
        "**",
        ["*";"*"],
        [Operator "*"; Operator "*"]
    );
    (
        // idx 36
        // PrefixLexer.prefixLex.037
        // One operator then one space
        "* ",
        ["*";" "],
        [Operator "*"]
    );

    (
        // idx 37
        // PrefixLexer.prefixLex.038
        // One whitespace then one decimal digit
        " 2",
        [" ";"2"],
        [Integer "2"]
    );
    (
        // idx 38
        // PrefixLexer.prefixLex.039
        // One whitespace then one operator
        " (",
        [" ";"("],
        [OpenParen]
    );
    (
        // idx 39
        // PrefixLexer.prefixLex.040
        // One whitespace then one close paren
        " )",
        [" ";")"],
        [CloseParen]
    );
    (
        // idx 40
        // PrefixLexer.prefixLex.041
        // One whitespace then one comma
        " ,",
        [" ";","],
        [Comma]
    );
    (
        // idx 41
        // PrefixLexer.prefixLex.042
        // One whitespace then one operator
        " *",
        [" ";"*"],
        [Operator "*"]
    );
    (
        // idx 42
        // PrefixLexer.prefixLex.043
        // Two whitewhitespace
        "  ",
        [" ";" "],
        []
    );
    (
        // idx 43
        // PrefixLexer.prefixLex.044
        // One alpha character
        // throws System.Exception "Unparsed input"
        "a",
        ["a"],
        []
    );
    (
        // idx 44
        // PrefixLexer.prefixLex.045
        // One alpha character
        "1 ",
        ["1"; " "],
        [Integer "1"]
    );
    |]

[<Test>]
[<TestCase(0, TestName = "PrefixLexer.prefixLex.01")>]
[<TestCase(1, TestName = "PrefixLexer.prefixLex.02")>]
[<TestCase(2, TestName = "PrefixLexer.prefixLex.03")>]
[<TestCase(3, TestName = "PrefixLexer.prefixLex.04")>]
[<TestCase(4, TestName = "PrefixLexer.prefixLex.05")>]
[<TestCase(5, TestName = "PrefixLexer.prefixLex.06")>]
[<TestCase(6, TestName = "PrefixLexer.prefixLex.07")>]
[<TestCase(7, TestName = "PrefixLexer.prefixLex.08")>]
[<TestCase(8, TestName = "PrefixLexer.prefixLex.09")>]
[<TestCase(9, TestName = "PrefixLexer.prefixLex.010")>]
[<TestCase(10, TestName = "PrefixLexer.prefixLex.011")>]
[<TestCase(11, TestName = "PrefixLexer.prefixLex.012")>]
[<TestCase(12, TestName = "PrefixLexer.prefixLex.013")>]
[<TestCase(13, TestName = "PrefixLexer.prefixLex.014")>]
[<TestCase(14, TestName = "PrefixLexer.prefixLex.015")>]
[<TestCase(15, TestName = "PrefixLexer.prefixLex.016")>]
[<TestCase(16, TestName = "PrefixLexer.prefixLex.017")>]
[<TestCase(17, TestName = "PrefixLexer.prefixLex.018")>]
[<TestCase(18, TestName = "PrefixLexer.prefixLex.019")>]
[<TestCase(19, TestName = "PrefixLexer.prefixLex.020")>]
[<TestCase(20, TestName = "PrefixLexer.prefixLex.021")>]
[<TestCase(21, TestName = "PrefixLexer.prefixLex.022")>]
[<TestCase(22, TestName = "PrefixLexer.prefixLex.023")>]
[<TestCase(23, TestName = "PrefixLexer.prefixLex.024")>]
[<TestCase(24, TestName = "PrefixLexer.prefixLex.025")>]
[<TestCase(25, TestName = "PrefixLexer.prefixLex.026")>]
[<TestCase(26, TestName = "PrefixLexer.prefixLex.027")>]
[<TestCase(27, TestName = "PrefixLexer.prefixLex.028")>]
[<TestCase(28, TestName = "PrefixLexer.prefixLex.029")>]
[<TestCase(29, TestName = "PrefixLexer.prefixLex.030")>]
[<TestCase(30, TestName = "PrefixLexer.prefixLex.031")>]
[<TestCase(31, TestName = "PrefixLexer.prefixLex.032")>]
[<TestCase(32, TestName = "PrefixLexer.prefixLex.033")>]
[<TestCase(33, TestName = "PrefixLexer.prefixLex.034")>]
[<TestCase(34, TestName = "PrefixLexer.prefixLex.035")>]
[<TestCase(35, TestName = "PrefixLexer.prefixLex.036")>]
[<TestCase(36, TestName = "PrefixLexer.prefixLex.037")>]
[<TestCase(37, TestName = "PrefixLexer.prefixLex.038")>]
[<TestCase(38, TestName = "PrefixLexer.prefixLex.039")>]
[<TestCase(39, TestName = "PrefixLexer.prefixLex.040")>]
[<TestCase(40, TestName = "PrefixLexer.prefixLex.041")>]
[<TestCase(41, TestName = "PrefixLexer.prefixLex.042")>]
[<TestCase(42, TestName = "PrefixLexer.prefixLex.043")>]
[<TestCase(43, TestName = "PrefixLexer.prefixLex.044", ExpectedException=typeof<System.Exception>, ExpectedMessage = "Unparsed input")>]
[<TestCase(44, TestName = "PrefixLexer.prefixLex.045")>]
let ``function lex - type string`` idx =
    let (externalForm, _, _) = prefixLexStringTypeValues.[idx]
    let (_, internalForm, _) = prefixLexStringTypeValues.[idx]
    let (_, _, currentResult) = prefixLexStringTypeValues.[idx]

    // Verify function input form and human form match.
    let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm
//    printfn "external form: %A" externalForm
//    printfn "internal form: %A" internalForm
//    printfn "converted form: %A" convertedForm
    Assert.AreEqual(convertedForm, internalForm)

    // Verify result of function
    let stringParser : string list -> token list = ArithmeticExpressionEvaluator.PrefixLexer.prefixLex
    let current = stringParser internalForm
//    printfn "expected result: %A" currentResult
//    printfn "function result: %A" current
    Assert.AreEqual(current, currentResult)

//#endregion


