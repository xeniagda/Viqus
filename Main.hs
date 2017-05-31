import Base
import Parser
import Generator

import System.IO
import Data.List
import Data.Foldable

main = do
    codeInf <- hGetContents stdin
    let code = foldr' (:) [] codeInf
        tokens = tokenize code
    let ast_ = makeAst Nothing tokens
    case ast_ of
        Ok (ast, _) ->
            let lines = makeCode ast
                code = intercalate "\n" lines
            in putStrLn $ code
        Err err -> hPutStrLn stderr $ "Code couldn't compile! Error: " ++ err
