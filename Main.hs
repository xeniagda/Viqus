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
    debug $ "Tokens:   " ++ intercalate " " tokens
    let ast_ = makeAst tokens
        at_ = pr2Map makeAT ast_
        code_ = prMap makeCode at_
    case ( ast_, at_, code_ ) of
        (Ok ast, Ok at, Ok code) ->
            do 
                debug $ "Ast:\n" ++ ( intercalate "\n" $ prettify ast )
                debug $ "AT: " ++ show at
                putStrLn $ intercalate "\n" code
        (_, _, Err err) -> hPutStrLn stderr $ "Code couldn't compile! Error: " ++ err
