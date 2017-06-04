{-
 - Generator - makes AST into actual code
 -}
module Generator where

import Base
import Parser
import ActionTree

defaultCode :: [String]
defaultCode =
    makeCode $ ATBlock
    [ ATAssign (localFlag ++ "print") (AEExpr "print")
    ]

makeCode :: AT -> [String] -- Ast to list of lines
makeCode x =
    case x of
        ATAssign var expr -> [var ++ "=" ++ makeExpr expr]
        ATExpr_ expr -> [makeExpr expr]
        ATWhile cond code -> ("while " ++ makeExpr cond ++ ":") : indent (makeCode code)
        ATIf cond code -> ("if " ++ makeExpr cond ++ ":") : indent (makeCode code)

        ATFuncDef funcName [arg] code ->
            ["def " ++ funcName ++ paren arg ++ ":"] ++
            (indent $ makeCode code)
        ATFuncDef funcName (arg:args) code ->
            ["def " ++ funcName ++ paren arg ++ ":"] ++
            (indent $ makeCode (ATFuncDef "a" args code)) ++
            (indent $ ["return a"])

        ATSimpleFuncDef funcName [arg] code ->
            ["def " ++ funcName ++ paren arg ++ ":"] ++
            (indent $ ["return " ++ makeExpr code])
        ATSimpleFuncDef funcName (arg:args) code ->
            ["def " ++ funcName ++ paren arg ++ ":"] ++
            (indent $ makeCode (ATSimpleFuncDef "a" args code)) ++
            (indent $ ["return a"])

        ATReturn exp -> ["return " ++ makeExpr exp]
        ATBlock xs -> concatMap makeCode xs

makeExpr :: ATExpr -> String -- Makes ATExpr into actual code. Should parenthesise expressions automaticallyexpressions automatically
makeExpr x =
    case x of
        AEExpr ex -> ex
        AEVar v -> v
        AEBinOp op x y -> paren $ makeExpr x ++ op ++ makeExpr y
        AEFuncApplic f [] -> makeExpr f
        AEFuncApplic f args -> (makeExpr $ AEFuncApplic f $ init args) ++ paren (makeExpr $ last args)