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

module ArithmeticExpressionEvaluator.Semantic.Tests

open ArithmeticExpressionEvaluator.Semantic

open NUnit.Framework

//#region "eval tests"

let evalValues : (expr * int)[] = [|
    (
        // idx 0
        // Semantic.eval.01
        Sum(1,1),
        2
    );
    (
        // idx 1
        // Semantic.eval.02
        Difference(4,1),
        3
    );
    (
        // idx 2
        // Semantic.eval.03
        Product(2,2),
        4
    );
    (
        // idx 3
        // Semantic.eval.04
        Quotient(4,2),
        2
    );
    |]
   
[<Test>]
[<TestCase(0, TestName = "Semantic.eval.01")>]
[<TestCase(1, TestName = "Semantic.eval.02")>]
[<TestCase(2, TestName = "Semantic.eval.03")>]
[<TestCase(3, TestName = "Semantic.eval.04")>]
let ``function mk_precedence`` idx =
    let (expr, _) = evalValues.[idx]
    let (_, result) = evalValues.[idx]

//    printfn "expr: %A" expr
 
    let functionResult = eval expr
//    printfn "expected result: %A" result
//    printfn "function result: %A" functionResult
    Assert.AreEqual(result, functionResult)
    
//#endregion