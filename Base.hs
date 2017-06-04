module Base where

import Data.Maybe

data ParseResult a
    = Ok a
    | Err String
    deriving Show

-- Chain parseresults:
prMap :: (a -> b) -> ParseResult a -> ParseResult b -- Function with argument
prMap f a =
    case a of
        Ok x -> Ok $ f x
        Err x -> Err x

prMap_ :: (a -> ParseResult b) -> ParseResult a -> ParseResult b -- Function that may also fail
prMap_ f a =
    case a of
        Ok x -> f x
        Err x -> Err x

prMap2 :: (a -> b -> c) -> ParseResult a -> ParseResult b -> ParseResult c -- Function that takes 2 arguments
prMap2 f a b =
    case (a, b) of
        (Ok x, Ok y) -> Ok $ f x y
        (Err e, _) -> Err e
        (_, Err e) -> Err e

prCollect :: [ParseResult x] -> ParseResult [x]
prCollect [] = Ok []
prCollect (x:xs) =
    case (x, prCollect xs) of
        (Ok x_, Ok xs_) -> Ok $ x_ : xs_
        (Err e, _) -> Err e
        (_, Err e) -> Err e

indent :: [String] -> [String] -- Indents all the lines by four spaces
indent = map ("    " ++)

-- Variant of map that passes each element's index as a second argument to f. Stolen from https://stackoverflow.com/a/16192050/1753929
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

mapIndMaybe :: (a -> Int -> Maybe b) -> [a] -> [b] -- Same as above but uses a mapMaybe
mapIndMaybe f l = mapMaybe (uncurry f) (zip l [0..])

paren :: String -> String
paren x = "(" ++ x ++ ")"

