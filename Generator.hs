{-
 - Generator - makes AST into actual code
 -}
module Generator where

import Base
import Parser


indent :: [String] -> [String] -- Indents all the lines by one space
indent = map ("    " ++)

makeCode :: Ast -> [String] -- Ast to list of lines
makeCode x =
    case x of
        Expr x -> [x]
        BinOp op param1 param2 -> ["(" ++ (head $ makeCode param1) ++ ")" ++ op ++ "(" ++ (head $ makeCode param2) ++ ")"]
        FuncApplic fn param -> [(head $ makeCode fn) ++ "(" ++ (head $ makeCode param) ++ ")"]
        Block [x] -> makeCode x
        Block (x:xs) -> makeCode x ++ makeCode (Block xs)