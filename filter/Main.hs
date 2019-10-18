module Main where

import qualified Data.Text        as T
import           Text.Pandoc.JSON

import           Displace

main :: IO ()
main = toJSONFilter replaceVariablesInStrings

replaceVariablesInStrings :: Inline -> IO Inline
replaceVariablesInStrings (Str s) = do
  replacedText <- replaceVariables $ T.pack s
  return $ Str $ T.unpack replacedText
replaceVariablesInStrings x = return x
