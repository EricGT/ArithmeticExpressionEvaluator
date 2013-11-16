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

module ArithmeticExpressionEvaluator.Semantic

type expr =
    | Sum of int * int
    | Product of int * int
    | Difference of int * int
    | Quotient of int * int

let eval (e : expr) =
    match e with
    | Sum (a,b) -> a + b
    | Product (a,b) -> a* b
    | Difference (a,b) -> a - b
    | Quotient (a,b) -> a / b