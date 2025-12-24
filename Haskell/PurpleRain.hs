{-# OPTIONS_GHC -Wall #-}

module Main
where

import qualified Data.Array.Unboxed as A (UArray, bounds, ixmap, listArray)
import Data.Array.Base (foldlArray)

type Problem = A.UArray Int Int

type Solution = (Int, Int)


parse :: String -> Problem
parse str = 
    let rain_drops = [if c == 'B' then 1 else negate 1 | c <- head . lines $ str]
        len = length rain_drops
    in A.listArray (1, len) rain_drops
    

solve :: Problem -> Solution
solve p =
    snd
    . minimum
    $ [(negate $ abs_diff a b, (a,b)) | a <- [l..u], b <- [a..u]]
    where
        (l, u) = A.bounds p
        abs_diff :: Int -> Int -> Int
        abs_diff from to = abs . foldlArray (+) 0 $ A.ixmap (from,to) id p


format :: Solution -> String
format (a,b) = unwords . map show $ [a,b]


main :: IO ()
main = do
    interact $ format . solve . parse
