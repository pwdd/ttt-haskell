module TTT.Console.IO.IO (printer, reader) where

printer :: String -> IO ()
printer = putStr

reader :: IO String
reader = getLine

