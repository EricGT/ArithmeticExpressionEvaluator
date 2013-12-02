#load "Init.fsx"

//let externalForm = "1+2";;
//let externalForm = "2*3";;
//let externalForm = "2*3+4";;
//let externalForm = "2*3+4*5";;
//let externalForm = "2*(3+4)*5";;

//let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm;;
//let lexResult = ArithmeticExpressionEvaluator.ShuntingYard.infixLex convertedForm;;
//let precedenceClimbingResult = ArithmeticExpressionEvaluator.PrecedenceClimbing.precedenceClimbing lexResult;;

//let externalForm = "1+2";;
//let externalForm = "2*3";;

// Test of ShuntingYard
//let convertedForm = ArithmeticExpressionEvaluator.Lib.explode externalForm;;
//let lexResult = ArithmeticExpressionEvaluator.ShuntingYard.lex convertedForm;;
//let shuntingYardResult = ArithmeticExpressionEvaluator.ShuntingYard.ShuntingYard lexResult;;

let externalForm = "1+2"
//let externalForm = "3-1"
//let externalForm = "4"

let internalForm = ArithmeticExpressionEvaluator.Lib.explode externalForm;;
let lexResult = ArithmeticExpressionEvaluator.PrefixLexer.prefixLex internalForm;;
let parserResult = ArithmeticExpressionEvaluator.InfixFactoredGrammar.infixFactoredGrammar_b lexResult;;
