module Main
where

import Data.ByteString.Lazy.Char8 as B ( interact )  
import Data.ByteString.Lazy as B (ByteString)

type Problem  = Int
type Solution = Int


parse :: ByteString -> Problem
parse = undefined


solve :: Problem -> Solution
solve = undefined


format :: Solution -> ByteString
format solution = undefined


main :: IO ()
main = do
    B.interact $ format . solve . parse
