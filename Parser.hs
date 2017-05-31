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

import Data.List
import Data.Char
import Data.Maybe

import System.IO

import Debug.Trace

-- Break a string into tokens
tokenize :: String -> [String]
tokenize code =
    let stripped = dropWhile isSpace code
    in if length stripped == 0
        then []
        else
            let
                maybeFirstToken = takeWhile isAlphaNum stripped
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


-- Generate an AST from tokens
data Ast
    = Expr String
    | BinOp String Ast Ast
    deriving Show


makeAst :: Maybe (Ast, Int) -> [String] -> Maybe (Ast, Int) -- Syntax tree, amount of tokens swallowed
makeAst before tokens
    | tokens == [] = Nothing
    | otherwise =
        case before of
            Just (ast, len) ->
                let op = head tokens
                in case makeAst Nothing $ tail tokens of
                    Nothing -> Nothing
                    Just (other, len_) -> Just ( BinOp op ast other, len + len_ + 1 )
            Nothing ->
                case makeBasicAst tokens of
                    Nothing -> Nothing
                    Just (before_, len) ->
                        if len == length tokens
                            then Just (before_, len)
                            else makeAst (Just (before_, len)) $ drop len tokens

makeBasicAst :: [String] -> Maybe (Ast, Int)
makeBasicAst tokens
    | tokens == [] = Nothing
    | all isAlphaNum $ head tokens = Just ( Expr $ head tokens, 1 )
    | head tokens == "(" = -- Find closing paren
        let parens = map (\c -> if c == "(" then 1 else if c == ")" then -1 else 0) tokens
            runningSums = scanl1 (+) parens
            parensEnd = findIndex (==0) runningSums
            in case parensEnd of
                Just end ->
                    case makeAst Nothing $ take (end - 1) $ tail tokens of
                        Just (codeBetween, len) -> if len == end - 1 then Just ( codeBetween, len + 2 ) else Nothing
                        Nothing -> Nothing
                Nothing -> Nothing
    | otherwise = Nothing


main = do
    putStr "> "
    hFlush stdout
    line <- getLine
    let tokens = tokenize line
    putStrLn "Tokens:"
    putStrLn $ intercalate "\n" $ map ("    "++) tokens
    putStrLn "\nAst:"
    putStrLn $ show $ makeAst Nothing tokens
    main
