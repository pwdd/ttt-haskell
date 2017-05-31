module TTT.Console.IO.IOContext (IOContext(..)) where

import TTT.Messenger.Messenger

data IOContext = IOContext { reader :: IO String
                           , printer :: String -> IO ()
                           , clear :: Int -> IO ()
                           , messenger :: Messenger
                           }

