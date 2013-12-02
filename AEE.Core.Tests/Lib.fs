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

module ArithmeticExpressionEvaluator.Lib.Tests

open ArithmeticExpressionEvaluator.Lib

open NUnit.Framework

//#region "explode tests"

[<Test>]
let private explodeValues : (string * string list)[] = [|
    (
        // idx 0
        // Lib.explode.01
        // empty string using double quotes
        "",
        []
    );
    (
        // idx 1
        // Lib.explode.02
        // one character string
        "a",
        ["a"]
    );
    (
        // idx 2
        // Lib.explode.03
        // two character string
        "ab",
        ["a"; "b"]
    );
    (
        // idx 3
        // Lib.explode.04
        // three character string
        "abc",
        ["a"; "b"; "c"]
    );
    (
        // idx 4
        // Lib.explode.05
        // A full word
        "example",
        ["e"; "x"; "a"; "m"; "p"; "l"; "e"]
    );
    (
        // idx 5
        // Lib.explode.06
        // A small sentence.
        "A small sentence.",
        ["A"; " "; "s"; "m"; "a"; "l"; "l"; " "; "s"; "e"; "n"; "t"; "e"; "n"; "c"; "e"; "."]
    );
    |]

[<Test>]
[<TestCase(0, TestName = "Lib.explode.01")>]
[<TestCase(1, TestName = "Lib.explode.02")>]
[<TestCase(2, TestName = "Lib.explode.03")>]
[<TestCase(3, TestName = "Lib.explode.04")>]
[<TestCase(4, TestName = "Lib.explode.05")>]
[<TestCase(5, TestName = "Lib.explode.06")>]
let ``function explode`` idx =
    let (input, _) = explodeValues.[idx]
    let (_, result) = explodeValues.[idx]
    let explodeResult = ArithmeticExpressionEvaluator.Lib.explode input
//    printfn "%A" explodeResult
    Assert.AreEqual(result, explodeResult)

// Note: The max length of a .NET string is
//   Int32.MaxValue
//   = 2^31 - 1
//   = 2,147,483,547
// .NET strings are unicode so use two bytes for each character.
// This is about 4GB of memory for a max string.

let private explodeLimitValues : (int)[] = [|
    (
        // idx 0
        // Lib.explode.101
        // string of length Int16.MaxValue
        int(System.Int16.MaxValue)
    );
    (
        // idx 1
        // Lib.explode.102
        // string of length UInt16.MaxValue
        int(System.UInt16.MaxValue)
    );
    (
        // idx 2
        // Lib.explode.103
        // string of length Int32.MaxValue
        // Long running
        // throws System.OutOfMemoryException
        int(System.Int32.MaxValue)
    );
    |]

// Note: If using code coverage tools, the code coverage tools may not
// understand NUnit Category attribute.
// Note: The System.Int32.MaxValue test is commented out because it is long running.
[<Test>]
[<TestCase(0, TestName = "Lib.explode.101", Category = "Limits")>]
[<TestCase(1, TestName = "Lib.explode.102", Category = "Limits")>]
//[<TestCase(2, TestName = "Lib.explode.103", ExpectedException=typeof<System.OutOfMemoryException>, Category = "Limits")>]
let ``function explode limits`` idx =
    let (size) = explodeLimitValues.[idx]
    let input = lazy (String.replicate size "a")
    ArithmeticExpressionEvaluator.Lib.explode (input.Force ()) |> ignore

//#endregion

