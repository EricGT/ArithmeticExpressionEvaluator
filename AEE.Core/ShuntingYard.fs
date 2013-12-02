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

module ArithmeticExpressionEvaluator.ShuntingYard

open ArithmeticExpressionEvaluator.Lib
open ArithmeticExpressionEvaluator.Semantic
open ArithmeticExpressionEvaluator.ParserCombinator
open ArithmeticExpressionEvaluator.PrefixLexer

//#region Infix Lexer

type token =
    | Integer of string
    | Operator of string
    | OpenParen
    | CloseParen

// stringof (p : 'a -> string * 'a) : ('a -> string * 'a)  // generic
// stringof (p : string list -> string * string list) : (string list -> string * string list)  // string
let stringof p = atleast 1 p |>> List.reduceBack (+)

// integer : string list -> token * string list
let integer = stringof (some isdecimaldigit) |>> (fun x -> Integer x)

// openParen : string list -> token * string list
let openParen = a "(" |>> (fun x -> OpenParen)

// closeParen : string list -> token * string list
let closeParen = a ")" |>> (fun x -> CloseParen)

// operator : string list -> token * string list
let operator = some isoperator |>> (fun x -> Operator x)

// rawtoken : string list -> token * string list
let rawtoken =
    integer <|>
    openParen <|>
    closeParen <|>
    operator

// spacedtoken : string list -> token * string list
let spacedtoken = many (some iswhitespace) .>>. rawtoken |>> snd

// tokens (sl : string list) : token list * string list
let rec tokens sl =
    try
        // get token from head of list
        let (t,rst) = spacedtoken sl
        // get tokens from tail of list
        let toks,rst1 = tokens rst
        // combine tokens into token list
        let (tokenlist,rest) = t::toks,rst1
        // return token list and remaining input characters as a tuple
        tokenlist,rest
    with
    | Noparse -> [],sl

//infixLex (sl : string list) : token list
let infixLex sl =
    // ending (prs : 'a -> 'b * string list) : 'a -> 'b
    let ending prs =
        fst << ((prs .>>. many(some iswhitespace) .>>. finished) |>> (fst << fst))
    ending tokens sl

//#endregion

let precedence = System.Collections.Generic.Dictionary<string,int>()
precedence.Add("+",2)
precedence.Add("-",2)
precedence.Add("*",3)
precedence.Add("/",3)
precedence.Add("^",4)

type assoc =
    | Right
    | Left

let associtivity = System.Collections.Generic.Dictionary<string,assoc>()
associtivity.Add("+",Left)
associtivity.Add("-",Left)
associtivity.Add("*",Left)
associtivity.Add("/",Left)
associtivity.Add("^",Right)

//#region Shunting Yard Algorithm

// Converts infix tokens to postfix tokens.

let ShuntingYard (l : token list) : token list =
    let rec read tokens (oq : token list) (s : token list) =
        match tokens with
        | (Integer v)::tokensTail ->
            let oq = (Integer v)::oq
            read tokensTail oq s
        | (Operator o1)::tokensTail ->
            let rec processStack (oq : token list) (s : token list) =
                match s with
                | o2t::stackTail ->
                    match o2t with
                    | Operator o2 ->
                        if (precedence.Item(o1) <= precedence.Item(o2))
                        then
                            let oq = o2t::oq
                            processStack oq stackTail
                    | _ -> ()
                | [] -> ()
            processStack oq s
            let s = (Operator o1)::s
            read tokensTail oq s
        | OpenParen::tokensTail ->
            let s = OpenParen::s
            read tokensTail oq s
        | CloseParen::tokensTail ->
            let rec processStack (oq : token list) (s : token list) =
                match s with
                | OpenParen::stackTail -> read tokensTail oq stackTail
                | CloseParen::stackTail -> processStack oq stackTail
                | (Operator x)::stackTail ->
                    let oq = (Operator x)::oq
                    processStack oq stackTail
                | (Integer x)::stackTail -> raise Noparse
                | [] -> raise Noparse
            processStack oq s
        | [] ->
            let rec processStack (oq : token list) (s : token list) =
                match s with
                | OpenParen::stackTail ->
                    raise Noparse
                | CloseParen::stackTail ->
                    raise Noparse
                | (Operator x)::stackTail ->
                    let oq = (Operator x)::oq
                    processStack oq stackTail
                | [] -> List.rev oq
                | _ -> raise Noparse
            let result = processStack oq s
            result
    read l [] []

//#endregion

//#region Postfix evaluator

let postfixEval (l : token list) : int =
    let rec processExpression (l : token list) (s : int list) =
        match l with
        | Integer x::tokensTail ->
            let value = System.Int32.Parse(x)
            let s = value::s
            processExpression tokensTail s
        | Operator o::tokensTail ->
            match s with
            | r::l::stackTail ->
                let value =
                    match o with
                    | "+" -> l + r
                    | "-" -> l - r
                    | "*" -> l * r
                    | "/" -> l / r
                    | "^" -> pown l r
                    | _ -> raise Noparse
                let s = value::stackTail
                processExpression tokensTail s
            | _ -> raise Noparse
        | [] ->
            List.head s
        | _ ->
            raise Noparse
    processExpression l []

//#endregion

//#region Shunting Yard Algorithm for AST

// Coverts infix tokens to expr i.e. prefix AST

let ShuntingYardAst (l : token list) : expr =
    let rec read tokens (s : token list) (es : expr list) : expr =
        match tokens with
        | (Integer v)::tokensTail ->
            let es = Int (System.Int32.Parse(v))::es
            read tokensTail s es
        | (Operator o1)::tokensTail ->
            let rec processStack (s : token list) (es : expr list) =
                match s with
                | o2t::stackTail ->
                    match o2t with
                    | Operator o2 ->
                        if (precedence.Item(o1) <= precedence.Item(o2))
                        then
                            let es =
                                match es with
                                | r::l::tail ->
                                    match o2 with
                                    | "+" -> [Sum(l,r)]
                                    | "-" -> [Difference(l,r)]
                                    | "*" -> [Product(l,r)]
                                    | "/" -> [Quotient(l,r)]
                                    | "^" -> [Power(l,r)]
                                    | _ -> raise Noparse
                                | _ -> raise Noparse
                            processStack stackTail es
                    | _ -> ()
                | [] -> ()
            processStack s es
            let s = (Operator o1)::s
            read tokensTail s es
        | OpenParen::tokensTail ->
            let s = OpenParen::s
            read tokensTail s es
        | CloseParen::tokensTail ->
            let rec processStack (s : token list) (es : expr list) =
                match s with
                | OpenParen::stackTail ->
                    read tokensTail stackTail es
                | CloseParen::stackTail ->
                    processStack stackTail es
                | (Operator x)::stackTail ->
                    match es with
                    | r::l::tail ->
                        match x with
                        | "+" ->
                            let es = (Sum(l,r)::tail)
                            processStack stackTail es
                        | "-" ->
                            let es = (Difference(l,r)::tail)
                            processStack stackTail es
                        | "*" ->
                            let es = (Product(l,r)::tail)
                            processStack stackTail es
                        | "/" ->
                            let es = (Quotient(l,r)::tail)
                            processStack stackTail es
                        | "^" ->
                            let es = (Power(l,r)::tail)
                            processStack stackTail es
                        | _ -> raise Noparse
                    | [] -> List.head es
                    | _ -> raise Noparse
                | (Integer x)::stackTail -> raise Noparse
                | [] -> raise Noparse
            processStack s es
        | [] ->
            let rec processStack (s : token list)  (es : expr list) =
                match s with
                | OpenParen::stackTail -> raise Noparse
                | CloseParen::stackTail -> raise Noparse
                | (Operator x)::stackTail ->
                    match es with
                    | r::l::tail ->
                        match x with
                        | "+" ->
                            let es = (Sum(l,r)::tail)
                            processStack stackTail es
                        | "-" ->
                            let es = (Difference(l,r)::tail)
                            processStack stackTail es
                        | "*" ->
                            let es = (Product(l,r)::tail)
                            processStack stackTail es
                        | "/" ->
                            let es = (Quotient(l,r)::tail)
                            processStack stackTail es
                        | "^" ->
                            let es = (Power(l,r)::tail)
                            processStack stackTail es
                        | _ -> raise Noparse
                    | _ -> raise Noparse
                | [] -> List.head es
                | _ -> raise Noparse
            let result = processStack s es
            result
    read l [] []

//#endregion