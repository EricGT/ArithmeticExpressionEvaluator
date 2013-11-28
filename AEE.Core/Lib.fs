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

module ArithmeticExpressionEvaluator.Lib

(* ------------------------------------------------------------------------- *)
(* List basics.                                                              *)
(* ------------------------------------------------------------------------- *)

// map (f : 'a -> 'b) : ('a list -> 'b list)
let map f =
    let rec mapf l =
        match l with
        | [] -> []
        | (x::t) -> 
            let y = f x
            y::(mapf t)
    mapf

// last (l : 'a list) : 'a
let rec last l =
    match l with
    | [x] -> x
    | (h::t) -> last t
    | [] -> failwith "last"
    
// butlast (l : 'a list) : 'a list
let rec butlast l =
    match l with
    | [_] -> []
    | (h::t) -> h::(butlast t)
    | [] -> failwith "butlast"

// rev : ('a list -> 'a list)
let rev =
    let rec rev_append acc l =
        match l with
        | [] -> acc
        | h::t -> rev_append (h::acc) t
    fun l -> rev_append [] l

//#region String operations

(* ------------------------------------------------------------------------- *)
(* String operations (surely there is a better way...)                       *)
(* ------------------------------------------------------------------------- *)

// Note: Other versions of explode will return a char list.
// Here the result is string list with each char being converted to a string
// containing a single character.

// explode (s : string) : string list
let explode (s : string) =
    let rec exap n (l : string list) : string list =
        if n < 0 
        then l 
        else exap (n - 1) ((s.Substring (n, 1))::l)
    exap (s.Length - 1) []

//#endregion

(* ------------------------------------------------------------------------- *)
(* Various useful list operations.                                           *)
(* ------------------------------------------------------------------------- *)

// partition (p : 'a -> bool) (l : 'a list) :  ('a list * 'a list)
let rec partition p l =
    match l with
    | [] -> [],l
    | h::t -> 
        let yes,no = partition p t
        if p(h) 
        then (if LanguagePrimitives.PhysicalEquality yes t then l,[] else h::yes,no)
        else (if LanguagePrimitives.PhysicalEquality no t then [],l else yes,h::no)

(* ------------------------------------------------------------------------- *)
(* Zipping, unzipping etc.                                                   *)
(* ------------------------------------------------------------------------- *)

// unzip : ('a * 'b) list -> 'a list * 'b list
let rec unzip =
    function 
    | [] -> [],[]
    | ((a,b)::rest) -> 
        let alist,blist = unzip rest
        (a::alist,b::blist)
