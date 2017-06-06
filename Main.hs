import Base
import AST
import AT
import Generator

import System.Environment
import System.IO
import Data.List
import Data.Foldable

main = run =<< getArgs

run args = do
    let debugLevel = getDebugLevel args
        debug level x = if level <= debugLevel then hPutStrLn stderr x else return ()

    inHandle <- getInHandle args
    outHandle <- getOutHandle args

    codeInf <- hGetContents inHandle
    let code = foldr' (:) [] codeInf
        tokens = tokenize code

    debug 3 $ intercalate " " tokens ++ " <-- Tokens"

    case makeAst tokens of
        Err e -> hPutStrLn stderr $ "AST failed: " ++ e
        Ok ast -> do 
            debug 2 "Made Ast"
            debug 3 $ "Ast raw: " ++ show ast
            debug 3 $ "Ast unparsed: " ++ unParse ast
            debug 3 $ "Ast ascii:\n" ++ ( intercalate "\n" $ indent $ prettify ast )
            case makeAT ast of
                Err e -> hPutStrLn stderr $ "AT failed: " ++ e
                Ok at -> do
                    debug 2 "Compiled code"
                    debug 3 $ "AT raw: " ++ show at
                    debug 3 $ "AT ascii:\n" ++ ( intercalate "\n" $ indent $ prettifyAT at )
                    debug 1 "Generated output."
                    hPutStrLn outHandle $ intercalate "\n" $ defaultCode ++ makeCode at

    hClose inHandle
    hClose outHandle

getInHandle :: [String] -> IO Handle
getInHandle ("-i" : path : _) = openFile path ReadMode
getInHandle (_ : rest) = getInHandle rest
getInHandle [] = return stdin

getOutHandle :: [String] -> IO Handle
getOutHandle ("-o" : path : _) = openFile path WriteMode
getOutHandle (_ : rest) = getOutHandle rest
getOutHandle [] = return stdout

getDebugLevel :: [String] -> Int
getDebugLevel ("-d" : "high" : _) = 3
getDebugLevel ("-d" : "medium" : _) = 2
getDebugLevel ("-d" : "low" : _) = 1
getDebugLevel ("-d" : "off" : _) = 0
getDebugLevel (_ : rest) = getDebugLevel rest
getDebugLevel [] = 1
