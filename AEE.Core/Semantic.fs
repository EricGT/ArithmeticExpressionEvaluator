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
    | Neg of expr
    | Fact of expr

let rec eval (e : expr) =
    match e with
    | Int l -> l
    | Sum (l,r) -> (eval l) + (eval r)
    | Product (l,r) -> (eval l) * (eval r)
    | Difference (l,r) -> (eval l) - (eval r)
    | Quotient (l,r) -> (eval l) / (eval r)
    | Power (l,r) -> pown (eval l) (eval r)
    | Neg(l) ->
        let v = (eval l)
        if v > 0
        then v * -1
        else v
    | Fact(l) ->
        let rec fact n acc =
            match n with
            | 0 -> acc
            | _ when n > 0 -> fact (n-1) (acc * n)
            | _ -> 0
        fact (eval l) 1
