module Main
where
import qualified Data.Map as Map
import Data.List (sortBy, nub, splitAt)
import Data.ByteString.Lazy.Char8 as B (words, unwords, readInt, lines, unlines, take, singleton, interact, pack)

import Data.ByteString.Lazy as B (ByteString)
import Data.Maybe (fromMaybe, fromJust)


type Point = (Int,Int)
type PtSet = [Point]
type Problem  = [PtSet]
type Solution = [PtSet]

parseline :: ByteString -> (Int, Int)
parseline s =
    let take2 :: [Int] -> (Int, Int)
        take2 (a:b:_) = (a,b)
        take2 _ = error "can't extract 2 numbers from string = "
    in take2 . map (fst . fromJust . B.readInt) . B.words $ s


parse :: ByteString -> Problem
parse =
    go . B.lines
    where
        go :: [ByteString] -> Problem
        go [] = error "unexpected end of input"
        go input@(l:ls)
            | len == 0 = []
            | otherwise = map parseline problem : go remaining
            where
                len :: Int
                len = fst . fromJust . B.readInt $ l
                (problem, remaining) = splitAt len ls


convexHull :: [Point] -> [Point]
convexHull ps
    | null other = [(x0,y0)]
    | otherwise  = reverse [(x+x0, y+y0) | (x,y) <- walk [(0,0)] other]
    where
        angle :: (Int, Int) -> Float
        angle (x,y) = atan2 (fromIntegral x :: Float) (fromIntegral y :: Float)
        findStart :: [Point] -> Point
        findStart ps =
            let min_y = minimum [y | (_,y) <- ps]
                min_x = minimum [x | (x,y) <- ps, y == min_y]
            in (min_x, min_y)
        start@(x0,y0) = findStart ps
        other :: [Point]
        other =
            map snd
            . sortBy (\(a1,_) (a2,_) -> compare a2 a1)
            . Map.toList
            . foldl (\m (k, a) -> Map.insertWith insert_f k a m) Map.empty
            . map (\p -> (angle p, p))
            . filter (/= (0,0))
            . nub
            . map (\(x,y) -> (x-x0,y-y0))
            $ ps
        insert_f :: (Int, Int) -> (Int,Int) -> (Int,Int)
        insert_f p1@(x1,y1) p2@(x2,y2)
            | (abs x1 + abs y1) > (abs x2 + abs y2) = p1
            | otherwise = p2
        walk :: [Point] -> [Point] -> [Point] -- Comment
        walk [] _ = error "there must be at least one element in the stack"
        walk stack [] = stack
        walk [s] (o:os) = walk (o:[s]) os
        walk stack@(s0:s1:_) others@(o:os)
            | ccw s1 s0 o = walk (o:stack) os
            | otherwise = walk (tail stack) others
        ccw :: Point -> Point -> Point -> Bool
        ccw (x1,y1) (x2,y2) (x3,y3) = (x2-x1) * (y3-y1) - (y2-y1) * (x3-x1) > 0


solve :: Problem -> Solution
solve = map convexHull


format :: Solution -> ByteString
format solution =
    B.unlines . concat $
        [(B.pack . show $ length ptset) : [format_pair pt | pt <- ptset] | ptset <- solution]
    where
        format_pair :: (Int, Int) -> ByteString
        format_pair (x,y) = B.unwords $ map (B.pack . show) [x,y]


main :: IO ()
main = do
    B.interact $ format . solve . parse
    --input <- B.getContents
    --putStr . format . solve . parse $ input