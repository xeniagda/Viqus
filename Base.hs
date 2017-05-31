module Base where


data Ast
    = Expr String
    | BinOp String Ast Ast
    | FuncApplic Ast Ast
    | Block [Ast]
    deriving Show

data ParseResult a
    = Ok a
    | Err String
    deriving Show

