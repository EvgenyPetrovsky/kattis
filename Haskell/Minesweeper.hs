{-# OPTIONS_GHC -Wall #-}

module Main
where

import Data.Set (Set)
import qualified Data.Set as Set

type Row = Int
type Col = Int

data Problem = Minefield 
    { rows :: Row
    , cols :: Col
    , mines :: Set (Row, Col)
    }

type Solution = [[Char]]


parse :: String -> Problem
parse input = 
    let (rows,cols) = read2ints . head $ lines input
        mines = Set.fromList . map read2ints . tail $ lines input
    in Minefield {rows = rows, cols = cols, mines = mines}
    where 
        read2ints :: String -> (Int, Int)
        read2ints str = 
            case map read . words $ str of
                (a:b:_) -> (a,b)
                _ -> error $ "incorrect input: " ++ str


solve :: Problem -> Solution
solve Minefield {cols = _cols, rows = _rows, mines = _mines} =
    [[ fill_rc (r,c) | c <- [1.._cols]] | r <- [1.._rows]]
    where 
        fill_rc :: (Row, Col) -> Char
        fill_rc rc
            | rc `Set.member` _mines = '*' 
            | otherwise = '.'



format :: Solution -> String
format = unlines


main :: IO ()
main = do
    interact $ format . solve . parse
