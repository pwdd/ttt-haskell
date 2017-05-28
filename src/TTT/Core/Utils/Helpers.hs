module TTT.Core.Utils.Helpers (chunks) where

import Data.List as List

chunks :: Int -> [a] -> [[a]]
chunks chunkSize list = takeWhile (not . null) $ List.unfoldr (Just . splitAt chunkSize) list

