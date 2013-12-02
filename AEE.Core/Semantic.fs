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

module ArithmeticExpressionEvaluator.Semantic

type expr =
    | Int of int
    | Sum of expr * expr
    | Product of expr * expr
    | Difference of expr * expr
    | Quotient of expr * expr
    | Power of expr * expr

let rec eval (e : expr) =
    match e with
    | Int a -> a
    | Sum (a,b) -> (eval a) + (eval b)
    | Product (a,b) -> (eval a) * (eval b)
    | Difference (a,b) -> (eval a) - (eval b)
    | Quotient (a,b) -> (eval a) / (eval b)
    | Power (a,b) -> pown (eval a) (eval b)