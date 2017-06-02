import Base
import Parser
import ActionTree
import Generator

import System.IO
import Data.List
import Data.Foldable

main = do
    codeInf <- hGetContents stdin
    let code = foldr' (:) [] codeInf
        tokens = tokenize code
    hPutStrLn stderr $ show tokens
    let ast_ = prMap fst $ makeAst Nothing tokens
        at_ = pr2Map makeAT ast_
        code_ = prMap makeCode at_
    case ( ast_, at_, code_ ) of
        (Ok ast, Ok at, Ok code) ->
            do 
                hPutStrLn stderr $ "Ast:\n" ++ ( intercalate "\n" $ prettify ast )
                hPutStrLn stderr $ "AT: " ++ show at
                putStrLn $ intercalate "\n" code
        (_, _, Err err) -> hPutStrLn stderr $ "Code couldn't compile! Error: " ++ err
