module ActionTree where

import Base
import Parser
import Data.List
import Data.Maybe

data AT -- Action tree
    = ATAssign String ATExpr
    | ATExpr_ ATExpr
    | ATWhile ATExpr AT
    | ATIf ATExpr AT
    | ATBlock [AT]
    deriving Show


data ATExpr
    = AEExpr String
    | AEBinOp String ATExpr ATExpr
    | AEFuncApplic ATExpr ATExpr
    deriving Show

-- Convert Ast's to ATExpr's
makeATExpr :: Ast -> ParseResult ATExpr
makeATExpr tree =
    case tree of
        Expr ex -> Ok $ AEExpr ex
        BinOp st x y -> prMap2 (AEBinOp st) (makeATExpr x) (makeATExpr y)
        FuncApplic f x -> prMap2 AEFuncApplic (makeATExpr f) (makeATExpr x)
        _ -> Err $ "Not an expression:\n" ++ (intercalate "\n" $ indent $ prettify tree)

-- Convert Ast's to AT's
makeAT :: Ast -> ParseResult AT
makeAT tree =
    case tree of
        BinOp s x_ y ->
            case x_ of
                Expr x ->
                    if s == "=" then
                        prMap (ATAssign x) (makeATExpr y)
                    else Err $ "Not an assignment:\n" ++ (intercalate "\n" $ prettify tree)
                _ -> Err $ "Not a variable:\n" ++ (intercalate "\n" $ prettify tree)
        Block asts ->
            let ats = map makeAT asts
                nonErrors = mapMaybe (\x ->
                    case x of
                        Err _ -> Nothing
                        Ok x -> Just x
                    ) ats
            in case find isErr ats of
                Just x ->
                    case x of
                        Err e -> Err e
                        _ -> Err "Something weird happened"
                Nothing -> Ok $ ATBlock nonErrors
        FuncApplic (FuncApplic (Expr "while") cond) code ->
            prMap2 ATWhile (makeATExpr cond) (makeAT code)
        FuncApplic (FuncApplic (Expr "if") cond) code ->
            prMap2 ATIf (makeATExpr cond) (makeAT code)
        _ -> prMap (ATExpr_) (makeATExpr tree)
