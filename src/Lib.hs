{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( replaceVariables
  ) where

import           Control.Exception
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe, listToMaybe)
import qualified Data.Text            as T
import           Text.Regex.TDFA      ((=~))
import           Text.Regex.TDFA.Text ()

newtype VariableNotDefinedException =
  VariableNotDefinedException T.Text

instance Show VariableNotDefinedException where
  show (VariableNotDefinedException var) =
    "Variable not defined: " ++ T.unpack var

instance Exception VariableNotDefinedException

-- | Matches all allowed filenames in a unix system, minus the ) character.
-- That is, all characters except "/" and ")".
filenameRegex :: T.Text
filenameRegex = "\\(\\(([^/)]+)\\)\\)"

-- | Finds the first variable name enclosed in (()) and returns the text
-- preceding it, the variable name, and the text after it, wrapped in a Right
-- value. If the text does not contain double parenthesises, returns the text
-- wrapped in a Left value.
takeFirstVarName ::
     T.Text -- ^ The text where variables are searched
  -> Either T.Text (T.Text, T.Text, T.Text) -- ^ The remaining text, or a tuple
  -- of (preceding text, variable name, remaining text)
takeFirstVarName text =
  let (precedingText, _, rest, submatches) =
        text =~ filenameRegex :: (T.Text, T.Text, T.Text, [T.Text])
   in case listToMaybe submatches of
        Nothing      -> Left precedingText
        Just varName -> Right (precedingText, varName, rest)

replaceFirstVariable ::
     Map.Map T.Text T.Text -> T.Text -> (T.Text, Maybe T.Text)
replaceFirstVariable varMap text =
  case takeFirstVarName text of
    Left remainingText -> (remainingText, Nothing)
    Right (precedingText, variableName, remainingText) ->
      let value = fromMaybe "" (Map.lookup variableName varMap)
       in (T.append precedingText value, Just remainingText)

replaceVariables :: Map.Map T.Text T.Text -> T.Text -> T.Text
replaceVariables varMap text =
  case replaceFirstVariable varMap text of
    (replacedText, Nothing) -> replacedText
    (replacedText, Just remainingText) ->
      T.append replacedText (replaceVariables varMap remainingText)
