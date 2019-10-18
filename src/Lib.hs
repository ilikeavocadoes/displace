{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( replaceVariables
  ) where

import           Control.Exception
import qualified Data.HashMap.Strict  as HMap
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe, listToMaybe)
import qualified Data.Text            as T
import           Data.Yaml            (Object, ParseException, Value (Object),
                                       decodeFileThrow)
import           Text.Regex.TDFA      ((=~))
import           Text.Regex.TDFA.Text ()

newtype VariableNotDefinedException =
  VariableNotDefinedException T.Text

instance Show VariableNotDefinedException where
  show (VariableNotDefinedException var) =
    "Variable not defined: " ++ T.unpack var

instance Exception VariableNotDefinedException

data VariablePath = VariablePath
  { filename   :: T.Text
  , pathInFile :: Maybe T.Text
  }

data FileContent =
  FileContent T.Text
              (Either ParseException Value)

data LookupError =
  FileNotRead

variableLookup ::
     Map.Map T.Text FileContent -> VariablePath -> Either LookupError T.Text
variableLookup variableMap VariablePath { filename = filename
                                        , pathInFile = pathInFile
                                        } =
  case Map.lookup filename variableMap of
    Nothing -> Left FileNotRead
    Just (FileContent textualContent eitherYamlContent) ->
      case pathInFile of
        Nothing -> Right textualContent
        Just varPath ->
          case eitherYamlContent of
            Right yamlValue -> Right $ getQualifiedYamlValue varPath yamlValue
            Left yamlParseException -> throw yamlParseException

getQualifiedYamlValue :: T.Text -> Value -> T.Text
getQualifiedYamlValue variablePath yamlValue =
  let pathParts = parseVariablePath variablePath
   in getQualifiedYamlValue' pathParts yamlValue

getQualifiedYamlValue' [] value = T.pack $ show value
getQualifiedYamlValue' (p:ps) value =
  case value of
    Object obj ->
      case (HMap.lookup p obj) of
        Just val -> getQualifiedYamlValue' ps val
        Nothing  -> throw $ VariableNotDefinedException p
    _ -> throw $ VariableNotDefinedException p

parseVariablePath :: T.Text -> [T.Text]
parseVariablePath variablePath = T.splitOn "." variablePath

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
