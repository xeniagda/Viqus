module Base where

import Data.Maybe

data ParseResult a
    = Ok a
    | Err String
    deriving Show

isErr :: ParseResult x -> Bool
isErr x =
    case x of
        Err _ -> True
        _ -> False

prMap :: (a -> b) -> ParseResult a -> ParseResult b
prMap f a =
    case a of
        Ok x -> Ok $ f x
        Err x -> Err x

pr2Map :: (a -> ParseResult b) -> ParseResult a -> ParseResult b
pr2Map f a =
    case a of
        Ok x -> f x
        Err x -> Err x

prMap2 :: (a -> b -> c) -> ParseResult a -> ParseResult b -> ParseResult c
prMap2 f a b =
    case (a, b) of
        (Ok x, Ok y) -> Ok $ f x y
        (Err e, _) -> Err e
        (_, Err e) -> Err e

indent :: [String] -> [String] -- Indents all the lines by one space
indent = map ("    " ++)

