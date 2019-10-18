module Main where

import qualified Data.Text.IO       as IO
import           Data.Yaml
import           Lib
import           System.Environment

main :: IO ()
main = IO.getContents >>= replaceVariables >>= IO.putStr
