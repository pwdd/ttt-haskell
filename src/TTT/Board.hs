module TTT.Board (
                    emptySpot
                  , newBoard
                  , placeMarker
                  , isFull
                  , isSpotAvailable
                  , availableSpots
                  , dimension
                  , rows
                  , columns
                  , diagonals
                  ) where

import Data.List as List
import qualified Data.Vector as Vector

import Types
import TTT.Utils.Helpers as Helpers

emptySpot :: Marker
emptySpot = ' '

newBoard :: Int -> Board
newBoard boardLength = replicate boardLength emptySpot

placeMarker :: Int -> Marker -> Board -> Board
placeMarker position marker board = take position board ++ marker : drop (position + 1) board

isFull :: Board -> Bool
isFull = notElem emptySpot

isSpotAvailable :: Board -> Int -> Bool
isSpotAvailable board spot = board !! spot == emptySpot

availableSpots :: Board -> [Int]
availableSpots = List.elemIndices emptySpot

dimension :: Board -> Int
dimension board = floor . sqrt $ (fromIntegral . length) board

length' :: Int -> Int
length' boardDimension = boardDimension * boardDimension

indices :: Int -> [Int]
indices n = [0..n - 1]

rows :: Int -> [[Int]]
rows boardDimension = Helpers.chunks boardDimension (indices $ length' boardDimension)

columns :: Int -> [[Int]]
columns boardDimension = transpose $ rows boardDimension

getNestedIndex :: [[Int]] -> (Int, Int) -> Int
getNestedIndex boardRows coordinate =
  nestedVector boardRows Vector.! fst coordinate Vector.! snd coordinate where
    nestedVector nestedList = Vector.map Vector.fromList (Vector.fromList nestedList)

diagonals :: Int -> [[Int]]
diagonals boardDimension =
  [
    makeDiagonal $ indices boardDimension
  , makeDiagonal $ reverse (indices boardDimension)
  ]
    where
    makeDiagonal diagonalIndices =
      map (getNestedIndex $ rows boardDimension) $ zip (indices boardDimension) diagonalIndices

