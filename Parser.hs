-- A simple programming language that compiles to Python 3

{- Example prorgam
 -
 -  x = 0;
 -  while (x < 20) {
 -      print(toString(x + 10));
 -  }
 -  print("Blast off!");
 -
 - Tokens:
 -  [x, =, 0, ; while, (, x, <, 20, ), {, print, (, toString, (, x, +, 10, ), ), }, ;, print, (, "Blast off!", ), ;]
 -
 - AST:
 -
 - [ Assign (Var "x") Expr "3"
 - , While (BinFunc ("<", Var "x", Expr "20")
 -         , [ FuncCall ("print", FuncCall ("toString", BinFunc ("+", Var "x", Expr "20")))
 -           ]
 -         )
 - , FuncCall ("print", Expr "\"Blast off!\"")
 - ]
 -
 - Generated Python code:
 -
 -  x = (3)
 -  while ((x)<(3)):
 -      print(toString((x)+(10)))
 -  print("Blast off")
 -}

module Parser where

import Base

import Data.List
import Data.Char
import Data.Maybe
import Debug.Trace

data TokenType
    = TSymbol
    | TOpenParen
    | TCloseParen
    | TBinOp
    | TSpace
    deriving Eq

getType :: String -> TokenType
getType x
    | all isAlphaNum x = TSymbol
    | all isSpace x = TSpace
    | elem (head x) "([{"= TOpenParen
    | elem (head x) ")]}"= TCloseParen
    | otherwise = TBinOp

getOtherParen :: String -> String
getOtherParen c
    | c == "(" = ")"
    | c == "{" = "}"
    | c == "[" = "]"
    | otherwise = c

-- Break a string into tokens

tokenize :: String -> [String]
tokenize code =
    let tok = partTokenize code
        grouped = groupBy (\a b -> getType a == getType b) tok
        binOpGrouped = concatMap (\x -> if (getType $ head x) == TBinOp then [concat x] else x) grouped
        noSpaces = filter ((/= TSpace) . getType) binOpGrouped
    in noSpaces

partTokenize :: String -> [String]
partTokenize code =
    let stripped = dropWhile isSpace code
    in if length stripped == 0
        then []
        else if stripped /= code then " " : partTokenize stripped
        else
            let maybeFirstToken = takeWhile isAlphaNum stripped
                firstToken =
                    if head stripped == '"'
                        then parseStr stripped
                        else if maybeFirstToken == []
                            then [head stripped]
                            else maybeFirstToken
                rest = drop (length firstToken) stripped
            in firstToken : tokenize rest

parseStr :: String -> String
parseStr code =
    let inString = takeWhile ((/=)'"') $ tail code
    in "\"" ++ inString ++ "\""

data Ast
    = Expr String
    | BinOp String Ast Ast
    | FuncApplic Ast Ast
    | Block [Ast]
    deriving Show


-- Generate an AST from tokens
makeAst :: Maybe (Ast, Int) -> [String] -> ParseResult (Ast, Int) -- Syntax tree, amount of tokens swallowed
makeAst before tokens
    | tokens == [] = Err "Unexpected end of feed"
    | otherwise =
        case before of
            Just (ast, len) ->
                let op = head tokens
                in if getType op == TBinOp
                    then case makeAst Nothing $ tail tokens of
                        Ok (other, len_) -> Ok ( BinOp op ast other, len + len_ + 1 ) -- Binary operation
                        Err err ->
                            case makeAst Nothing tokens of
                                Ok (other, len_) -> Ok (FuncApplic ast other, len + len_)
                                Err err_ -> Err err_
                    else case makeAst Nothing tokens of
                            Ok (other, len_) -> Ok (FuncApplic ast other, len + len_)
                            Err err_ -> Err err_
            Nothing -> makeBlock tokens

makeBlock :: [String] -> ParseResult (Ast, Int)
makeBlock tokens =
    case elemIndex ";" tokens of
        Nothing ->
            case makeBasicAst tokens of
                Err err -> Err err
                Ok (before_, len) ->
                    if len == length tokens
                        then Ok (before_, len)
                        else makeAst (Just (before_, len)) $ drop len tokens
        Just idx ->
            let start = take idx tokens
                end = drop (idx + 1) tokens
            in
                if length end == 0 then makeAst Nothing start
                else case (makeAst Nothing start, makeAst Nothing end) of
                    (Ok (b1, len1), Ok (b2, len2)) ->
                        case b2 of
                            Block xs -> Ok (Block $ b1 : xs, len1 + len2 + 1)
                            _ -> Ok (Block [b1, b2], len1 + len2 + 1)
                    _ ->
                        case makeBasicAst tokens of
                            Err err -> Err err
                            Ok (before_, len) ->
                                if len == length tokens
                                    then Ok (before_, len)
                                    else makeAst (Just (before_, len)) $ drop len tokens

makeBasicAst :: [String] -> ParseResult (Ast, Int)
makeBasicAst tokens
    | tokens == [] = Err "Unexpected end of feed"
    | isExpr $ head tokens = Ok ( Expr $ head tokens, 1 )
    | getType (head tokens) == TOpenParen = -- Find closing paren
        let parens = map (\c -> if c == head tokens then 1 else if c == getOtherParen (head tokens) then -1 else 0) tokens
            runningSums = scanl1 (+) parens
            parensEnd = findIndex (==0) runningSums
            in case parensEnd of
                Just end ->
                    case makeAst Nothing $ take (end - 1) $ tail tokens of
                        Err err -> Err err
                        Ok (codeBetween, len) ->
                            if len == end - 1 then Ok ( codeBetween, len + 2 ) else Err "Some error occured"
                Nothing -> Err "Mismatched parens"
    | otherwise = Err "Found nothing to parse"

isExpr :: String -> Bool
isExpr x
    | all isAlphaNum x = True
    | length x < 2 = False
    | head x == '"' && last x == '"' = True
    | otherwise = False

prettify :: Ast -> [String]
prettify ast =
    let (tx, tok, rst) =
            case ast of
                Expr e         -> ("Ex", e, [])
                BinOp op e1 e2 -> ("Bn", show op, [e1, e2])
                FuncApplic f a -> ("Fn", "", [f, a])
                Block as       -> ("Bl", "", as)
    in (tx ++ " " ++ tok)
     : concatMap (\lst ->
        let lines = prettify lst
        in ("|-" ++ head lines) : (map ("| " ++) $ tail lines))
       rst

