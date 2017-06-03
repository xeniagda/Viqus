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
    let ast_ = makeAst tokens
        at_ = pr2Map makeAT ast_
        code_ = prMap makeCode at_

    case ast_ of
        Ok ast -> do 
                debug $ "Ast:\n" ++ show ast
                debug $ "Ast:\n" ++ ( intercalate "\n" $ prettify ast )
        Err e -> debug e

    case at_ of
        Ok at -> debug $ show at
        Err e -> debug e

    case code_ of
        Ok code -> putStrLn $ intercalate "\n" code
        Err err -> hPutStrLn stderr $ "Code couldn't compile! Error: " ++ err
