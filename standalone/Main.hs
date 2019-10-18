module Main where

import qualified Data.Text.IO       as IO
import           Data.Yaml
import           System.Environment

import           Displace

main :: IO ()
main = IO.getContents >>= replaceVariables >>= IO.putStr
