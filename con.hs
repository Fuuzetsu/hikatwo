module Main (main) where 

import System.IO
import System.Environment

main = do
  args <- getArgs
  case length args /= 2 of
    True  -> return ()
    False -> do r <- readFile $ args !! 0
                writeFile (args !! 1) $ unlines . filter (\x -> x/= "") $ map (reverse . drop 1 . dropWhile (/= '\'') . reverse . drop 1 . dropWhile (/= '\'')) $ lines r
                
                