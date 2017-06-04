module Parser where

import Base

import Data.List
import Data.Char
import Data.Maybe
import Debug.Trace

orderOfOperations = [["=", ":"], ["==", "<", ">"], ["+", "-"], ["*", "/", "%"], ["^"], ["$"]]

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
    | elem (head x) "([{" = TOpenParen
    | elem (head x) ")]}" = TCloseParen
    | otherwise = TBinOp

-- Break a string into tokens
tokenize :: String -> [String]
tokenize code =
    let tok = partTokenize code
        grouped = groupBy (\a b -> getType a == getType b) tok
        binOpGrouped = concatMap (\x -> if (getType $ head x) == TBinOp then [concat x] else x) grouped
        noSpaces = filter ((/= TSpace) . getType) binOpGrouped
    in noSpaces

partTokenize :: String -> [String] -- Tokenizes without respecting grouped binary operators such as `==`
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
    let inString = takeWhile (/='"') $ tail code
    in "\"" ++ inString ++ "\""

data Ast
    = Expr String
    | BinOp String Ast Ast
    | FuncApplic Ast [Ast]
    | List [Ast]
    | Block [Ast]
    deriving Show


-- Generate an AST from tokens
makeAst :: [String] -> ParseResult Ast
makeAst tokens =
    let parens = map (\c -> if getType c == TOpenParen then 1 else if getType c == TCloseParen then -1 else 0) tokens
        running = scanl1 (+) parens
        paired = zip tokens running
        (semis : commas : ops) = -- Contains the indexed of the operations in order
            map (parseOps tokens) $ [[";"], [","]] ++ orderOfOperations
        operationsIdx = concat ops
    in case (semis, commas) of
        ([], []) -> case listToMaybe operationsIdx of
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
                                        Ok (f:args) -> Ok $ FuncApplic f args
                                        Err e -> Err e
                            else Err $ "Welp, nothing: " ++ (show tokens)

        ([], commas) ->
                let commas_ = sort commas
                    ranges = zip (map (1+) (-1 : commas_)) (commas_ ++ [length tokens])
                    funcs = map (\(s, e) -> drop s $ take e tokens) ranges
                    parsed = map makeAst funcs
                in case prCollect parsed of
                    Ok blk -> Ok $ List blk
                    Err e -> Err e

        (seps, _) -> 
                let seps_ = sort seps
                    ranges = zip (map (1+) (-1 : seps_)) (seps_ ++ [length tokens])
                    funcs = filter ((> 0) . length) $ map (\(s, e) -> drop s $ take e tokens) ranges
                    parsed = map makeAst funcs
                in case prCollect parsed of
                    Ok blk -> Ok $ Block blk
                    Err e -> Err e

parseOps :: [String] -> [String] -> [Int]
parseOps tokens ops =
    let parens = map (\c -> if getType c == TOpenParen then 1 else if getType c == TCloseParen then -1 else 0) tokens
        running = scanl1 (+) parens
        paired = zip tokens running
    in 
        (reverse . sort . concatMap (\op ->
            mapIndMaybe (\(ch, depth) idx -> if ch == op && depth == 0 then Just idx else Nothing) paired
        )) ops

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
                FuncApplic f a -> ("Fn", "", f : a)
                List items     -> ("[]", "", items)
                Block as       -> ("Bl", "", as)
    in case rst of
        [] -> [tx ++ " " ++ tok]
        rst -> 
            (tx ++ " " ++ tok) : 
                concatMap (\lst ->
                    let (first : rest) = prettify lst
                    in ("|-" ++ first) : (map ("| " ++) rest)
                ) (init rst) ++
                    let (first : rest) = prettify $ last rst
                    in ("|-" ++ first) : (map ("  " ++) rest)

-- Try to make an Ast back into actual code
unParse :: Ast -> String
unParse tree =
    case tree of
        Expr st -> st
        BinOp f x y -> paren $ unParse x ++ f ++ unParse y
        FuncApplic f args -> unParse f ++ " " ++ (intercalate " " $ map unParse args)
        List items -> paren $ intercalate ", " $ map unParse items
        Block lines -> "{" ++ (intercalate "; " $ map unParse lines) ++ "}"
