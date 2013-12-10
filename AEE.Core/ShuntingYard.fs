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
    | PrefixOperator of string
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
let rec inputTokens sl =
    try
        // get token from head of list
        let (t,rst) = spacedtoken sl
        // get tokens from tail of list
        let toks,rst1 = inputTokens rst
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
    ending inputTokens sl

//#endregion

type fixity =
    | Infix
    | Prefix
    | Postfix

type associtivity =
    | Left
    | Right

let operators = System.Collections.Generic.Dictionary<string,(fixity * associtivity * int) list>()
operators.Add("+",[(Infix,  Left, 2)])  // Prolog 500 yfx
operators.Add("-",[(Infix,  Left, 2);   // Prolog 500 yfx
                   (Prefix, Left, 2)])  // Prolog 500 fx
operators.Add("*",[(Infix,  Left, 3)])  // Prolog 400 yfx
operators.Add("/",[(Infix,  Left, 3)])  // Prolog 400 yfx
operators.Add("^",[(Infix,  Right,4)])  // Prolog 200 xfy
operators.Add("!",[(Postfix,Left, 5)])  // Prolog 100 xf

let isPrefix (op : string) : bool =
    let items = operators.Item(op)
    let rec isPrefix items =
        match items with
        | hd::tl ->
            match hd with
            | (Prefix,_,_) -> true
            | _ -> isPrefix tl
        | [] -> false
    isPrefix items

let isPostfix (op : string) : bool =
    let items = operators.Item(op)
    let rec isPostfix items =
        match items with
        | hd::tl ->
            match hd with
            | (Postfix,_,_) -> true
            | _ -> isPostfix tl
        | [] -> false
    isPostfix items

let associtivity (op : string) (fix : fixity) : associtivity =
    let items = operators.Item(op)
    let rec assoc items =
        match items with
        | hd::tl ->
            match hd with
            | (fixity,a,_) when fixity = fix -> a
            | _ -> assoc tl
        | [] -> failwithf "Unknown operator: %s %A" op fix
    assoc items

let precedence (op : string) (fix : fixity) : int =
    let items = operators.Item(op)
    let rec prec items =
        match items with
        | hd::tl ->
            match hd with
            | (fixity,_,p) when fixity = fix -> p
            | _ -> prec tl
        | [] -> failwithf "Unknown operator: %s %A" op fix
    prec items

//#region Shunting Yard Algorithm

// Converts infix tokens to postfix tokens.

let ShuntingYard (inputTokens : token list) : token list =

    let opType (inputTokens : token list) : token =
        match inputTokens with
        | (Operator op)::tl -> Operator op
        | (PrefixOperator op)::tl -> PrefixOperator op
        | _ -> failwith "Expected an operator."

    let rec prefixOperatorParser (inputTokens : token list) (outputQueue : token list) (operatorStack : token list) : (token list * token list) =
        match inputTokens with
        | (Operator op)::tokensTail when isPrefix op ->
            let operatorStack = (PrefixOperator op)::operatorStack
            prefixOperatorParser tokensTail outputQueue operatorStack
        | _ ->
            operandParser inputTokens outputQueue operatorStack

    and operandParser (inputTokens : token list) (outputQueue : token list) (operatorStack : token list) : (token list * token list) =
        match inputTokens with
        | (Integer v)::tokensTail ->
            let outputQueue = (Integer v)::outputQueue
            operandParser tokensTail outputQueue operatorStack
        | (Operator op)::tokensTail ->
            if isPostfix op
            then
                let outputQueue = (Operator op)::outputQueue
                postfixOperatorParser tokensTail outputQueue operatorStack
            else
                proceesPrefixOperator inputTokens outputQueue operatorStack
        | CloseParen::tokensTail ->
            proceesPrefixOperator inputTokens outputQueue operatorStack
        | OpenParen::tokensTail ->
            let operatorStack = OpenParen::operatorStack
            prefixOperatorParser tokensTail outputQueue operatorStack
        | PrefixOperator _::tokensTail -> raise Noparse
        | [] -> (outputQueue,operatorStack)

    and postfixOperatorParser (inputTokens : token list) (outputQueue : token list) (operatorStack : token list) : (token list * token list) =
        match inputTokens with
        | (Operator op)::stackTail when isPostfix op ->
            let outputQueue = (Operator op)::outputQueue
            postfixOperatorParser inputTokens outputQueue stackTail
        | _ -> proceesPrefixOperator inputTokens outputQueue operatorStack

    and proceesPrefixOperator (inputTokens : token list) (outputQueue : token list) (operatorStack : token list) : (token list * token list) =
        match operatorStack with
        | (PrefixOperator op)::stackTail ->
            let outputQueue = (PrefixOperator op)::outputQueue
            proceesPrefixOperator inputTokens outputQueue stackTail
        | _ -> operatorParser inputTokens outputQueue operatorStack

    and operatorParser (inputTokens : token list) (outputQueue : token list) (operatorStack : token list) : (token list * token list) =
        match inputTokens with
        | (Operator o1)::tokensTail
        | (PrefixOperator o1)::tokensTail->
            let op1 = opType inputTokens
            let rec processStack (outputQueue : token list) (operatorStack : token list) (op1 : token) : (token list * token list) =
                match operatorStack with
                | Operator o2::stackTail
                | PrefixOperator o2::stackTail ->
                    let cmp c1 c2 =
                        let assoc1 = associtivity c1 Infix
                        let prec1 = precedence c1 Infix
                        let prec2 = precedence c2 Infix
                        match assoc1 with
                        | Left -> prec2 >= prec1
                        | Right -> prec2 > prec1
                    if (cmp o1 o2)
                    then
                        let op2 = opType operatorStack
                        let outputQueue = op2::outputQueue
                        processStack outputQueue stackTail op1
                    else
                        let operatorStack = (Operator o1)::operatorStack
                        (outputQueue,operatorStack)
                | _ ->
                    let operatorStack = (Operator o1)::operatorStack
                    (outputQueue,operatorStack)
            let (outputQueue,s) = processStack outputQueue operatorStack op1
            prefixOperatorParser tokensTail outputQueue s
        | CloseParen::tokensTail ->
            let rec processStack (outputQueue : token list) (operatorStack : token list) : (token list * token list) =
                match operatorStack with
                | OpenParen::stackTail -> proceesPrefixOperator tokensTail outputQueue stackTail
                | CloseParen::stackTail -> processStack outputQueue stackTail
                | (Operator x)::stackTail ->
                    let outputQueue = (Operator x)::outputQueue
                    processStack outputQueue stackTail
                | (PrefixOperator x)::stackTail -> raise Noparse
                | (Integer x)::stackTail -> raise Noparse
                | [] -> raise Noparse
            processStack outputQueue operatorStack
        | Integer _::tokensTail -> raise Noparse
        | OpenParen::tokensTail -> raise Noparse
        | [] ->
            (outputQueue,operatorStack)
    let endParser (outputQueue : token list) (operatorStack : token list) : token list =
        let rec processStack (outputQueue : token list) (operatorStack : token list) : token list =
            match operatorStack with
            | OpenParen::stackTail -> raise Noparse
            | CloseParen::stackTail -> raise Noparse
            | (Operator x)::stackTail ->
                let outputQueue = (Operator x)::outputQueue
                processStack outputQueue stackTail
            | (PrefixOperator x)::stackTail ->
                let outputQueue = (PrefixOperator x)::outputQueue
                processStack outputQueue stackTail
            | [] -> List.rev outputQueue
            | _ -> raise Noparse
        let result = processStack outputQueue operatorStack
        result

    let (outputQueue,s) = prefixOperatorParser inputTokens [] []
    endParser outputQueue s

//#endregion

//#region Postfix evaluator

let postfixEval (tokens : token list) : int =
    let rec processExpression (tokens : token list) (operatorStack : int list) =
        match tokens with
        | Integer x::tokensTail ->
            let value = System.Int32.Parse(x)
            let operatorStack = value::operatorStack
            processExpression tokensTail operatorStack
        | Operator o::tokensTail ->
            match operatorStack with
            | r::l::stackTail ->
                // infix operator
                let value =
                    match o with
                    | "+" -> l + r
                    | "-" -> l - r
                    | "*" -> l * r
                    | "/" -> l / r
                    | "^" -> pown l r
                    | _ -> raise Noparse
                let operatorStack = value::stackTail
                processExpression tokensTail operatorStack
            | l::stackTail ->
                // postfix operator
                let value =
                    match o with
                    | "!" ->
                        let rec fact n acc =
                            match n with
                            | 0 -> acc
                            | _ when n > 0 -> fact (n-1) (acc * n)
                            | _ -> 0
                        fact l 1
                    | _ -> raise Noparse
                let operatorStack = value::stackTail
                processExpression tokensTail operatorStack
            | _ -> raise Noparse
        | PrefixOperator o::tokensTail ->
            match operatorStack with
            | l::stackTail ->
                // prefix operator
                let value =
                    match o with
                    | "-" ->
                        if l > 0
                        then l * -1
                        else l
                    | _ -> raise Noparse
                let operatorStack = value::stackTail
                processExpression tokensTail operatorStack
            | _ -> raise Noparse
        | [] ->
            List.head operatorStack
        | _ ->
            raise Noparse
    processExpression tokens []

//#endregion

//#region Shunting Yard Algorithm for AST

// Coverts postfix tokens to expr i.e. prefix AST

let ShuntingYardAst (inputTokens : token list) : expr =
    let rpnToAst (rpn : token list) : expr =
        let rec processRpn (tokens : token list) (exprStack : expr list) : expr =
            match tokens with
            | Integer x::tokensTail ->
                let value = Int (System.Int32.Parse(x))
                let exprStack = value::exprStack
                processRpn tokensTail exprStack
            | Operator o::tokensTail ->
                match exprStack with
                | l::r::stackTail ->
                    // infix operator
                    let value =
                        match o with
                        | "+" -> Sum(r,l)
                        | "-" -> Difference(r,l)
                        | "*" -> Product(r,l)
                        | "/" -> Quotient(r,l)
                        | "^" -> Power(r,l)
                        | _ -> raise Noparse
                    let exprStack = value::stackTail
                    processRpn tokensTail exprStack
                | l::stackTail ->
                    // postfix operator
                    let value =
                        match o with
                        | "!" ->Fact(l)
                        | _ -> raise Noparse
                    let exprStack = value::stackTail
                    processRpn tokensTail exprStack
                | _ -> raise Noparse
            | PrefixOperator o::tokensTail ->
                match exprStack with
                | l::stackTail ->
                    // prefix operator
                    let value =
                        match o with
                        | "-" -> Neg(l)
                        | _ -> raise Noparse
                    let exprStack = value::stackTail
                    processRpn tokensTail exprStack
                | _ -> raise Noparse
            | [] ->
                List.head exprStack
            | _ ->
                raise Noparse
        processRpn rpn []
    rpnToAst inputTokens

//#endregion

