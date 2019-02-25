module Main where

import qualified Data.Text.IO       as IO
import           Data.Yaml
import           Lib
import           System.Environment

main :: IO ()
main = do
  filename <- head <$> getArgs
  variableValues <- decodeFileThrow filename
  IO.interact $ replaceTextVars variableValues
