{-# OPTIONS_GHC -Wall #-}

module Main
where


type Problem = [Int]
type Solution = (Int, Int)


parse :: String -> Problem
parse str = [if c == 'B' then 1 else negate 1 | c <- head . lines $ str]
    
{-

plan:

1. find global minimum closest to the left
2. find global maximum closest to the left
3. identify which of them is located to the right - this is end point
4. identify start point by finding right-most point before end point in the given series that equals to global minimum/maximum located on the left

-}

solve :: Problem -> Solution
solve p =
    let scanned = tail $ scanl (+) 0 p
        zipped = zip scanned [1..]
        (_,min_idx) = minimum zipped
        (_,max_idx) = (\(v,i) -> (v,negate i)) . maximum . map (\(v,idx) -> (v,negate idx)) $ zipped
        start_idx = min min_idx max_idx
        end_idx = max min_idx max_idx
    in (if start_idx == 1 then 1 else start_idx + 1, end_idx)


format :: Solution -> String
format (a,b) = unwords . map show $ [a,b]


main :: IO ()
main = do
    interact $ format . solve . parse
