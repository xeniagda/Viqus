{-
 - Generator - makes AST into actual code
 -}
module Generator where

import Base
import Parser
import ActionTree

paren :: String -> String
paren x = "(" ++ x ++ ")"

makeCode :: AT -> [String] -- Ast to list of lines
makeCode x =
    case x of
        ATAssign var expr -> [var ++ "=" ++ paren (makeExpr expr)]
        ATExpr_ expr -> [makeExpr expr]
        ATWhile cond code -> ("while " ++ makeExpr cond ++ ":") : indent (makeCode code)
        ATIf cond code -> ("if " ++ makeExpr cond ++ ":") : indent (makeCode code)
        ATBlock xs -> concatMap makeCode xs

makeExpr :: ATExpr -> String -- Makes ATExpr into actual code. Should parenthesise expressions automaticallyexpressions automatically
makeExpr x =
    case x of
        AEExpr ex -> ex
        AEBinOp op x y -> paren $ makeExpr x ++ op ++ makeExpr y
        AEFuncApplic f x -> (makeExpr f) ++ (paren $ makeExpr x)