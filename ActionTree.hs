module ActionTree where

import Base
import Parser
import Data.List
import Data.Maybe
import Data.Char

localFlag :: String
localFlag = "L_" -- Placed in front of all variables and functions


data AT -- Action tree
    = ATAssign String ATExpr
    | ATExpr_ ATExpr
    | ATWhile ATExpr AT
    | ATIf ATExpr AT
    | ATFuncDef String [String] AT
    | ATSimpleFuncDef String [String] ATExpr
    | ATReturn ATExpr
    | ATBlock [AT]
    deriving Show


data ATExpr
    = AEExpr String
    | AEVar String
    | AEList [ATExpr]
    | AEBinOp String ATExpr ATExpr
    | AEFuncApplic ATExpr [ATExpr]
    deriving Show

isVar :: String -> Bool
isVar x
    | all (\x -> isDigit x || x == '.') x
        = False
    | length x > 1
        && head x == '"'
        && last x == '"'
        = False
    | otherwise
        = True

-- Convert Ast's to ATExpr's
makeATExpr :: Ast -> ParseResult ATExpr
makeATExpr tree =
    case tree of
        Expr ex ->
            if isVar ex
                then Ok $ AEVar $ localFlag ++ ex
                else Ok $ AEExpr ex

        BinOp "$" x_ y_ ->
            case (makeATExpr x_, makeATExpr y_) of
                (Ok x, Ok y) -> Ok $ AEFuncApplic x [y]
                (Err e, _) -> Err e
                (_, Err e) -> Err e

        BinOp st x y -> prMap2 (AEBinOp st) (makeATExpr x) (makeATExpr y)

        List items -> prMap AEList $ prCollect $ map makeATExpr items

        FuncApplic f args -> prMap2 AEFuncApplic (makeATExpr f) (prCollect $ map makeATExpr args)

        _ -> Err $ "Not an expression: `" ++ unParse tree ++ "`"

-- Convert Ast's to AT's
makeAT :: Ast -> ParseResult AT
makeAT tree =
    case tree of
        BinOp ":" x_ y_ ->
            case x_ of
                FuncApplic f_ args_ ->
                    let parsed = map (\x_ ->
                                case makeATExpr x_ of
                                    Ok (AEVar x) -> Ok x
                                    _ -> Err $ unParse x_ ++ " is not a variable"
                                ) args_
                    in case (makeATExpr f_, prCollect parsed) of
                        (Err e, _) -> Err e
                        (_, Err e) -> Err e
                        (Ok (AEVar f), Ok args) -> prMap (ATFuncDef f args) $ makeAT y_

                _ -> Err $ "Not a variable: `" ++ unParse x_ ++ "` " ++ paren (show x_)

        BinOp "=" x_ y_ ->
            case makeATExpr x_ of
                Ok (AEVar x) ->
                    prMap (ATAssign x) $ makeATExpr y_

                Ok (AEFuncApplic (AEVar f) args_) ->
                    let parsed = map (\x_ ->
                                case x_ of
                                    AEVar x -> Ok x
                                    _ -> Err $ show x_ ++ " is not a variable"
                                ) args_
                    in case prCollect parsed of
                        Err e -> Err e
                        Ok args -> prMap (ATSimpleFuncDef f args) $ makeATExpr y_

                _ -> Err $ "Not a variable: `" ++ unParse x_ ++ "` " ++ paren (show x_)

        FuncApplic (Expr "while") [cond, code] ->
            prMap2 ATWhile (makeATExpr cond) $ makeAT code

        FuncApplic (Expr "if") [cond, code] ->
            prMap2 ATIf (makeATExpr cond) $ makeAT code

        FuncApplic (Expr "return") [exp] ->
            prMap ATReturn $ makeATExpr exp

        Block asts ->
            prMap ATBlock $ prCollect $ map makeAT asts

        _ -> prMap (ATExpr_) (makeATExpr tree)


-- Prettifiers:
prettifyAT :: AT -> [String]
prettifyAT ast =
    let (tx, rst) =
            case ast of
                ATAssign var e    -> ("= " ++ var, [prettifyATExpr e])
                ATExpr_ e         -> ("Exp", [prettifyATExpr e])
                ATWhile cond code -> ("While", [prettifyATExpr cond, prettifyAT code])
                ATIf cond code    -> ("If", [prettifyATExpr cond, prettifyAT code])
                ATFuncDef f args code -> ("def : " ++ f ++ " " ++ intercalate " " args, [prettifyAT code])
                ATSimpleFuncDef f args code -> ("def = " ++ f ++ " " ++ intercalate " " args, [prettifyATExpr code])
                ATReturn ex       -> ("Return", [prettifyATExpr ex])
                ATBlock code      -> ("Bl", map prettifyAT code)
    in case rst of
        [] -> [tx]
        rst ->
            tx : concatMap (\lst ->
                let (first : rest) = lst
                in ("|-" ++ first) : (map ("| " ++) rest)
            ) (init rst)
             ++ let (first : rest) = last rst
                in ("|-" ++ first) : (map ("  " ++) rest)

prettifyATExpr :: ATExpr -> [String]
prettifyATExpr ast =
    let (tx, rst) =
            case ast of
                AEExpr e         -> (" " ++ e, [])
                AEVar v          -> ("Var " ++ v, [])
                AEList items     -> ("[]", map prettifyATExpr items)
                AEBinOp op x y   -> (" " ++ op, [prettifyATExpr x, prettifyATExpr y])
                AEFuncApplic f x -> ("Fn", prettifyATExpr f : map prettifyATExpr x)
    in case rst of
        [] -> [tx]
        rst ->
            tx : concatMap (\lst ->
                let (first : rest) = lst
                in ("|-" ++ first) : (map ("| " ++) rest)
            ) (init rst)
             ++ let (first : rest) = last rst
                in ("|-" ++ first) : (map ("  " ++) rest)


