{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( replaceTextVars
  ) where

import           Control.Exception
import           Data.Attoparsec.Text
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import qualified Data.Text            as T

newtype VariableNotDefinedException =
  VariableNotDefinedException T.Text

instance Show VariableNotDefinedException where
  show (VariableNotDefinedException var) =
    "Variable not defined: " ++ T.unpack var

instance Exception VariableNotDefinedException

parseDoubleParens :: Parser T.Text
parseDoubleParens = do
  string "(("
  varName <- takeTill (== ')')
  string "))"
  return varName

parseCharOrValue = parseDoubleParens `eitherP` anyChar

parseSequence = many' parseCharOrValue

replaceVar dictionary (Right c) = T.singleton c
replaceVar dictionary (Left var) =
  fromMaybe (throw $ VariableNotDefinedException var) $
  Map.lookup var dictionary

replaceSequence :: Map.Map T.Text T.Text -> [Either T.Text Char] -> T.Text
replaceSequence dictionary sequence =
  T.concat $ map (replaceVar dictionary) sequence

replaceTextVars :: Map.Map T.Text T.Text -> T.Text -> T.Text
replaceTextVars dictionary text =
  case parseOnly parseSequence text of
    Left _    -> ""
    Right seq -> replaceSequence dictionary seq
