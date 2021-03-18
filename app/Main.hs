module Main where

import qualified Lib

main :: IO ()
main = putStrLn x
        where
            x = show (Lib.tally [])
