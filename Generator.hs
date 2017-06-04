{-
 - Generator - makes AST into actual code
 -}
module Generator where

import Base
import Parser
import ActionTree

import Data.List

defaultCode :: [String] -- Code in the beginning of all programs containing basic definitions
defaultCode =
    makeCode $ ATBlock
    [ ATAssign (localFlag ++ "print") (AEExpr "print")
    , ATAssign (localFlag ++ "int") (AEExpr "int")
    , ATAssign (localFlag ++ "str") (AEExpr "str")
    , ATAssign (localFlag ++ "input") (AEExpr "input")
    , ATAssign (localFlag ++ "len") (AEExpr "len")
    , ATAssign (localFlag ++ "at") (AEExpr "lambda x:lambda y:x[y]")
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

makeExpr :: ATExpr -> String -- Makes ATExpr into actual code. Should parenthesise expressions automatically
makeExpr x =
    case x of
        AEExpr ex -> ex
        AEVar v -> v
        AEList items -> "[" ++ intercalate "," (map makeExpr items) ++ "]"
        AEBinOp op x y -> paren $ makeExpr x ++ op ++ makeExpr y
        AEFuncApplic f [] -> makeExpr f
        AEFuncApplic f args -> (makeExpr $ AEFuncApplic f $ init args) ++ paren (makeExpr $ last args)