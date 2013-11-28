
printfn __SOURCE_DIRECTORY__;;

//#I @".\..\ArithmeticExpressionEvaluator\AEE.Core\bin\Debug"
#I @"bin\Debug"

#r @"AEE.Core.dll"

// Reduce margins
fsi.PrintWidth <- 72;;

open ArithmeticExpressionEvaluator.Lib;;
open ArithmeticExpressionEvaluator.Semantic;;
open ArithmeticExpressionEvaluator.ParserCombinator;;
open ArithmeticExpressionEvaluator.PrefixLexer;;
open ArithmeticExpressionEvaluator.Prefix;;
open ArithmeticExpressionEvaluator.InfixFactoredGrammar;;
open ArithmeticExpressionEvaluator.ShuntingYard;;
open ArithmeticExpressionEvaluator.PrecedenceClimbing;;
