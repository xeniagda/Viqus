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

orderOfOperations = [[";"], ["="], ["==", "<", ">"], ["+", "-"], ["*", "/", "%"], ["^"]]

data TokenType
    = TSymbol
    | TOpenParen
    | TCloseParen
    | TBinOp
    | TSpace
    deriving (Eq, Show)


getType :: String -> TokenType
getType x
    | isExpr x = TSymbol
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
makeAst :: [String] -> ParseResult Ast
makeAst tokens =
    let parens = map (\c -> if getType c == TOpenParen then 1 else if getType c == TCloseParen then -1 else 0) tokens
        running = scanl1 (+) parens
        paired = zip tokens running
        (semis : ops) = -- Contains the indexed of the operations in order
            map
                (reverse . sort . concatMap (\op ->
                    mapIndMaybe (\(ch, depth) idx -> if ch == op && depth == 0 then Just idx else Nothing) paired
                ))
                orderOfOperations
        operationsIdx = concat ops
    in case semis of
        [] -> case listToMaybe operationsIdx of
                Just idx ->
                    let x = makeAst $ take idx tokens
                        y = makeAst $ drop (idx + 1) tokens
                        f = tokens !! idx
                    in prMap2 (BinOp f) x y
                Nothing ->
                    if length tokens > 2 && (elemIndex 0 $ init running) == Nothing
                        then makeAst (tail $ init tokens)
                        else if length tokens > 0 && (isExpr $ head tokens)
                            then if length tokens == 1
                                then Ok $ Expr $ head tokens
                                else -- Functions
                                    let args = (map (1+) $ elemIndices 0 $ init running)
                                        ranges = zip (0 : args) (args ++ [length tokens])
                                        fns = map (\(s, e) -> drop s $ take e tokens) ranges
                                        parsed = prCollect $ map makeAst fns
                                    in case parsed of
                                        Ok args -> Ok $ foldl1 (FuncApplic) args
                                        Err e -> Err e
                            else Err $ "Welp, nothing: " ++ (show tokens)
        seps -> let seps_ = sort seps
                    ranges = zip (map (1+) (-1 : seps_)) (seps_ ++ [length tokens])
                    funcs = filter ((> 0) . length) $ map (\(s, e) -> drop s $ take e tokens) ranges
                    parsed = map makeAst funcs
                in case prCollect parsed of
                    Ok blk -> Ok $ Block blk
                    Err e -> Err e

code = ["1", "+", "(", "7", "/", "3", ")", "/", "1", "-", "f", "(", "x", "+", "1", ")"]

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

