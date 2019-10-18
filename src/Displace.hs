{-# LANGUAGE OverloadedStrings #-}

module Displace
  ( replaceVariables
  ) where

import           Control.Exception
import qualified Data.HashMap.Strict  as HMap
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe, listToMaybe)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Yaml            (Object, ParseException, Value (Object),
                                       decodeFileEither)
import qualified Data.Yaml            as Yaml
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

getQualifiedYamlValue' [] value = T.pack $ prettyPrintValue value
getQualifiedYamlValue' (p:ps) value =
  case value of
    Object obj ->
      case (HMap.lookup p obj) of
        Just val -> getQualifiedYamlValue' ps val
        Nothing  -> throw $ VariableNotDefinedException p
    _ -> throw $ VariableNotDefinedException p

prettyPrintValue :: Value -> String
prettyPrintValue (Yaml.Object obj)    = show obj
prettyPrintValue (Yaml.Array array)   = show array
prettyPrintValue (Yaml.String text)   = T.unpack text
prettyPrintValue (Yaml.Number number) = show number
prettyPrintValue (Yaml.Bool b)        = show b
prettyPrintValue Yaml.Null            = "null"

parseVariablePath :: T.Text -> [T.Text]
parseVariablePath variablePath = T.splitOn "." variablePath

-- | Matches all allowed filenames in a unix system, minus the ) character.
filenameRegex :: T.Text
filenameRegex = "\\(\\(([^)]+)\\)\\)"

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

data ReplaceResult
  = NeedFile T.Text
  | Replaced (T.Text, Maybe T.Text)

replaceFirstVariable :: Map.Map T.Text FileContent -> T.Text -> ReplaceResult
replaceFirstVariable fileMap text =
  case takeFirstVarName text of
    Left remainingText -> Replaced (remainingText, Nothing)
    Right (precedingText, variableName, remainingText) ->
      let (filename, colonAndPathInFile) = T.breakOn ":" variableName
          maybePathInFile =
            case colonAndPathInFile of
              ":" -> Nothing
              ""  -> Nothing
              s   -> Just $ T.drop 1 s
          variablePath =
            VariablePath {filename = filename, pathInFile = maybePathInFile}
          lookupResult = variableLookup fileMap variablePath
       in case lookupResult of
            Left FileNotRead -> NeedFile filename
            Right value ->
              Replaced (T.append precedingText value, Just remainingText)

replaceVariables :: T.Text -> IO T.Text
replaceVariables text = replaceVariables' (Map.empty) text

replaceVariables' :: Map.Map T.Text FileContent -> T.Text -> IO T.Text
replaceVariables' varMap text =
  case replaceFirstVariable varMap text of
    NeedFile filename -> do
      newMap <- loadNewFile varMap filename
      replaceVariables' newMap text
    Replaced (replacedText, Nothing) -> return replacedText
    Replaced (replacedText, Just remainingText) -> do
      rest <- replaceVariables' varMap remainingText
      return $ T.append replacedText rest

loadNewFile ::
     Map.Map T.Text FileContent -> T.Text -> IO (Map.Map T.Text FileContent)
loadNewFile oldMap filename = do
  decodeResult <- decodeFileEither $ T.unpack filename
  -- Every text file has a newline at the end so let's drop it
  textualContent <- T.dropEnd 1 <$> (TIO.readFile $ T.unpack filename)
  let content =
        case decodeResult of
          Left parseException ->
            FileContent textualContent (Left parseException)
          Right yamlValue -> FileContent textualContent (Right yamlValue)
  return $ Map.insert filename content oldMap
