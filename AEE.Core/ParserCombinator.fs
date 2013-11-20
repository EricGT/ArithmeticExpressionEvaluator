//#region License

(*

  (c) Copyright, Eric Taucher 2013

  Portions of the code from HOL Light licensed under:

       John Harrison, University of Cambridge Computer Laboratory
                                                                          
            (c) Copyright, University of Cambridge 1998
              (c) Copyright, John Harrison 1998-2007
                                                                           
    (See "HOL Light License.txt" for details.)

  Protions of the code:

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

module ArithmeticExpressionEvaluator.ParserCombinator

open ArithmeticExpressionEvaluator.Lib

//#region Basic parser combinators

(* ------------------------------------------------------------------------- *)
(* Basic parser combinators.                                                 *)
(* ------------------------------------------------------------------------- *)

/// Exception returned by parser combinators when an exception occurs during parsing.
exception Noparse

/// Parses any single item satisfying a predicate.

// some (p : 'a -> bool) (l : 'a list) : ('a * 'a list)                   // generic
// some (p : string -> bool) (l : string list) : (string * string list)   // string
// some (p : token  -> bool) (l : token list)  : (token  * token list)    // token

let some p l =
    match l with
    | [] -> raise Noparse
    | (h::t) -> 
        if p h 
        then (h,t) 
        else raise Noparse

/// Parser that requires a specific item.

// a (tok : 'a) : ('a list -> 'a * 'a list)                     // generic
// a (tok : string) : (string list -> string * string list)     // string
// a (tok : token)  : (token list  -> token  * token list)      // token

let a item = 
    some (fun x -> x = item)

/// Produce alternative composition of two parsers.
// NOTE : This is called (||) in the original HOL-Light code.

// (<|>) (parser1 : 'a -> 'b) (parser2 : 'a -> 'b) (input : 'a) : 'b                                                                                   // generic
// (<|>) (parser1 : string list -> string * string list) (parser2 : string list -> string * string list) (input : string list) : string * string list  // string
// (<|>) (parser1 : token list  -> token  * token list)  (parser2 : token list  -> token  * token list)  (input : token list)  : token  * token list   // token
 
let (<|>) parser1 parser2 input =
    try 
        parser1 input
    with 
    | Noparse -> parser2 input

/// Sequentially compose two parsers.
// NOTE : This is called (++) in the original HOL-Light code.

// (.>>.) (parser1 : 'a -> 'b * 'c) (parser2 : 'c -> 'd * 'e) (input : 'a) : ('b * 'd) * 'e                                                                         // generic
// (.>>.) (parser1 : string list -> string * string list) (parser2 : string list -> string * string list) (input : string list) : (string * string) * string list   // string
// (.>>.) (parser1 : token list  -> token  * token list)  (parser2 : token list  -> token  * token list)  (input : token list)  : (token  * token)  * token list    // token

let (.>>.) parser1 parser2 input =
    let result1,rest1 = parser1 input
    let result2,rest2 = parser2 rest1
    (result1,result2),rest2

/// Parses zero or more successive items using given parser.

// many (prs : 'a -> 'b * 'a) (input : 'a) : 'b list * 'a                                               // generic
// many (prs : string list -> string * string list) (input : string list) : string list * string list   // string
// many (prs : token list  -> token  * token list)  (input : token list)  : token list  * token list    // token

let rec many prs input =
    try 
        let result,next = prs input
        let results,rest = many prs next
        (result::results),rest
    with 
    | Noparse -> [],input

/// Apply function to parser result.

// NOTE : This is called (>>) in the original HOL-Light code.
// This has to be renamed to avoid collision with F# (>>).

// The main use of this function is to convert the output of a parser.
// When you make a parser you typically get differnt types of output, e.g.
// string list
// string
// string list * token)
// (token list * string list) * int
// but need the output to be something like a string, token or token list.
// So one creates a function to transform the output to the type desired, e.g. treatment.
// Then to get the correct output type of a parser one would use the parser as
// parser |>> treatment
// so that the parser pulls out the accepted values 
// and treatment converts it to the proper type.

let (|>>) prs treatment input =
    let result,rest = prs input
    treatment(result),rest

/// Parses at least a given number of successive items using given parser.

// atleast (n : int) (prs : 'a -> 'b * 'a) (i : 'a) : 'b list * 'a                                      // generic
// atleast (n : int) (prs : string list -> string * string list) (i : int) : string list * string list  // string
// atleast (n : int) (prs : token list  -> token  * token list)  (i : int) : token list  * token list   // token

let rec atleast n prs i =
    (if n <= 0 
     then many prs
     else prs .>>. atleast (n - 1) prs |>> (fun (h,t) -> h::t)) i

/// Parser that checks emptiness of the input.

// finished (input : 'a list) : int * 'a list          // generic
// finished (input : string list) : int * string list  // string
// finished (input : token list)  : int * token list   // token

let finished input =
    if input = [] 
    then 0,input 
    else failwith "Unparsed input"

//#endregion