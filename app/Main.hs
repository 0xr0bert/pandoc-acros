module Main where

import Lib
import Text.Pandoc.JSON ( toJSONFilter )

main :: IO ()
main = toJSONFilter processPandoc