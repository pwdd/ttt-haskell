module TTT.Console.IO.IO (printer, reader, clearScreen) where

import Control.Concurrent(threadDelay)

printer :: String -> IO ()
printer = putStr

reader :: IO String
reader = getLine

escape = "\ESC[2J\ESC[4;0H"

clearScreen :: Int -> IO ()
clearScreen microseconds = do
  threadDelay microseconds
  printer escape

