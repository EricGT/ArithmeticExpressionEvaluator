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

module ArithmeticExpressionEvaluator.PrefixLexer

open ArithmeticExpressionEvaluator.Lib
open ArithmeticExpressionEvaluator.ParserCombinator

//#region Character discrimination

(* ------------------------------------------------------------------------- *)
(* Character discrimination.                                                 *)
(* ------------------------------------------------------------------------- *)

// iswhitespace string : bool
// isparen string : bool
// isoperator string : bool
// isdecimaldigit string : bool
let iswhitespace,isparen,isoperator,isdecimaldigit =
    let charcode s = int ((s:string).Chars(0))
    let whitespace = " \t\n\r"
    let parens = "()"
    let operators = "*+-/"
    let decimaldigits = "0123456789"
    let allchars = whitespace + parens + operators + decimaldigits
    let ctable = Array.create 256 0
    List.iter (fun c -> Array.set ctable (charcode c) 1) (explode whitespace)
    List.iter (fun c -> Array.set ctable (charcode c) 2) (explode parens)
    List.iter (fun c -> Array.set ctable (charcode c) 4) (explode operators)
    List.iter (fun c -> Array.set ctable (charcode c) 8) (explode decimaldigits)
    let iswhitespace c = Array.get ctable (charcode c) = 1
    let isparen c  = Array.get ctable (charcode c) = 2
    let isoperator c = Array.get ctable (charcode c) = 4
    let isdecimaldigit c = Array.get ctable (charcode c) = 8
    iswhitespace,isparen,isoperator,isdecimaldigit

//#endregion

//#region Prefix Lexer

(* ------------------------------------------------------------------------- *)
(* Prefix Lexer.                                                              *)
(* ------------------------------------------------------------------------- *)

type token = 
    | Integer of string
    | Operator of string
    | OpenParen
    | CloseParen
    | Comma
    | WhiteSpace

// stringof (p : 'a -> string * 'a) : ('a -> string * 'a)  // generic
// stringof (p : string list -> string * string list) : (string list -> string * string list)  // string
let stringof p = atleast 1 p |>> List.reduceBack (+)

// integer : string list -> token * string list
let integer = stringof (some isdecimaldigit) |>> (fun x -> Integer x)

// openParen : string list -> token * string list
let openParen = a "(" |>> (fun x -> OpenParen)

// closeParen : string list -> token * string list
let closeParen = a ")" |>> (fun x -> CloseParen)

// comma : string list -> token * string list
let comma = a "," |>> (fun x -> Comma)

// operator : string list -> token * string list
let operator = some isoperator |>> (fun x -> Operator x)

// rawtoken : string list -> token * string list
let rawtoken =  
    integer <|>
    openParen <|>
    closeParen <|>
    comma <|>
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

//lex (sl : string list) : token list
let prefixLex sl =
    // ending (prs : 'a -> 'b * string list) : 'a -> 'b
    let ending prs =
        fst << ((prs .>>. many(some iswhitespace) .>>. finished) |>> (fst << fst))
    ending tokens sl

//#endregion
