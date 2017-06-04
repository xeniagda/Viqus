import Base
import Parser
import ActionTree
import Generator
import System.Environment

import System.IO
import Data.List
import Data.Foldable

main = run =<< getArgs

run args = do
    let debug x = if elem "-d" args then hPutStrLn stderr x else return ()

    codeInf <- hGetContents stdin
    let code = foldr' (:) [] codeInf
        tokens = tokenize code

    debug $ intercalate " " tokens ++ " <-- Tokens"

    case makeAst tokens of
        Err e -> hPutStrLn stderr $ "AST failed: " ++ e
        Ok ast -> do 
            debug $ "Ast raw: " ++ show ast
            debug $ "Ast unparsed: " ++ unParse ast
            debug $ "Ast ascii:\n" ++ ( intercalate "\n" $ indent $ prettify ast )
            case makeAT ast of
                Err e -> hPutStrLn stderr $ "AT failed: " ++ e
                Ok at -> do
                    debug $ "AT raw: " ++ show at
                    debug $ "AT ascii:\n" ++ ( intercalate "\n" $ indent $ prettifyAT at )
                    putStrLn $ intercalate "\n" $ defaultCode ++ makeCode at
