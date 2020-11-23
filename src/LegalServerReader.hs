{-# LANGUAGE OverloadedStrings #-}

module LegalServerReader where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack)
import Data.Char (digitToInt, isDigit)
import Data.Either
import Data.Ini
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Calendar as C (Day, fromGregorian)
import Data.Time.Format
  ( defaultTimeLocale,
    formatTime,
    parseTimeM,
  )
import Data.Time.Format.ISO8601 (ISO8601, iso8601ParseM)
import Network.HTTP.Req
  ( GET (GET),
    NoReqBody (NoReqBody),
    basicAuth,
    bsResponse,
    defaultHttpConfig,
    https,
    req,
    responseBody,
    responseStatusCode,
    runReq,
    (/:),
    (=:),
  )
import Text.Read (readMaybe)
import qualified Xeno.DOM as Xeno
import Xeno.Types (XenoException)

-- Need to request xml from server
-- need to then read the xml, creating database TimeSlips from the xml.

-- Datatype to hold credentials needed to access a report using the LegalServer Reports API.
data LegalServerCreds = LegalServerCreds
  { siteRoot :: T.Text,
    apiUser :: T.Text,
    apiPass :: T.Text,
    reportId :: T.Text,
    reportKey :: T.Text
  }

-- | Attempt to create LegalServerCreds from a list of text.
--    These creds will be needed to access reports.
credsFromList :: [T.Text] -> Either [String] LegalServerCreds
credsFromList credList =
  if Prelude.length credList /= 5
    then Left ["Need five values for legalserver credentials."]
    else
      let [siteRoot, apiUser, apiPass, reportId, reportKey] = credList
       in Right $ LegalServerCreds siteRoot apiUser apiPass reportId reportKey

requestReport :: LegalServerCreds -> IO B.ByteString
requestReport creds = runReq defaultHttpConfig $ do
  let site = siteRoot creds
      user = apiUser creds
      pass = apiPass creds
      id = reportId creds
      key = reportKey creds
  r <-
    req
      GET
      (https site /: "modules" /: "report" /: "api_export.php")
      NoReqBody
      bsResponse
      ( basicAuth
          (encodeUtf8 user)
          (encodeUtf8 pass)
          <> ("load" =: (id :: T.Text))
          <> ("api_key" =: (key :: T.Text))
      )
  liftIO $ print (responseStatusCode r :: Int)
  --return (decodeUtf8 (responseBody r))
  return $ responseBody r

type RowMap = M.Map [Char] (Maybe [Char])

lookupValueEither :: Either String Ini -> T.Text -> T.Text -> Either String T.Text
lookupValueEither iniOrError section key = case iniOrError of
  Left error -> Left error
  Right iniFile -> lookupValue section key iniFile

concatCreds ::
  [Either String T.Text] ->
  Either [String] [T.Text]
concatCreds eitherCreds = do
  let (errs, creds) = partitionEithers eitherCreds
  if not $ null errs then Left errs else Right creds

readCredentials :: String -> IO (Either [String] LegalServerCreds)
readCredentials credsPath = do
  credFile <- readIniFile credsPath
  let creds = concatCreds [lookupValueEither credFile "global" "site_root", lookupValueEither credFile "global" "api_user", lookupValueEither credFile "global" "api_pass", lookupValueEither credFile "Timeslips" "id", lookupValueEither credFile "Timeslips" "key"]
  case creds of
    Left errs -> return $ Left errs
    Right credList -> return $ credsFromList credList

getChildWithName :: Xeno.Node -> [Char] -> Maybe Xeno.Node
getChildWithName row lookingFor =
  let columns = Xeno.children row
      cells = filter (\n -> (unpack $ Xeno.name n) == lookingFor) columns
   in if length cells == 0 then Nothing else Just $ head cells

cleanContentShow :: Show a => a -> [Char]
cleanContentShow content = Prelude.takeWhile (/= '"') $ Prelude.drop 6 $ Prelude.show content

getChildText :: Xeno.Node -> Maybe [Char]
getChildText node =
  let stringContents = [cleanContentShow n | n@(Xeno.Text {}) <- Xeno.contents node]
   in if not $ null stringContents then Just $ concat stringContents else Nothing

getDateFromCell :: Xeno.Node -> String -> Maybe C.Day
getDateFromCell row column = do
  chars <- getCharsFromCell row column
  parseADate chars

getCharsFromCell :: Xeno.Node -> [Char] -> Maybe [Char]
getCharsFromCell row column = do
  cell <- getChildWithName row column
  getChildText cell

getIntFromCell :: Xeno.Node -> [Char] -> Maybe Integer
getIntFromCell row column = do
  chars <- getCharsFromCell row column
  readMaybe chars :: Maybe Integer

getTextFromCell :: Xeno.Node -> [Char] -> Maybe T.Text
getTextFromCell row column = do
  chars <- getCharsFromCell row column
  return $ T.pack chars

getFloatFromCell :: Xeno.Node -> [Char] -> Maybe Double
getFloatFromCell row column = do
  chars <- getCharsFromCell row column
  readMaybe chars :: Maybe Double

parseReportColumn :: Xeno.Node -> RowMap -> RowMap
parseReportColumn node rowmap =
  let columnName = unpack $ Xeno.name node
      columnValue = getChildText node --getTextFromCell node columnName
   in M.insert columnName columnValue rowmap

-- Parse a row of a legalserver report into a Map from the column name
-- to the value as Text
parseReportRow :: Xeno.Node -> RowMap
parseReportRow node = foldr parseReportColumn M.empty (Xeno.children node)

parseLSReport :: B.ByteString -> Either XenoException [RowMap]
parseLSReport rawXML =
  let parsed = Xeno.parse rawXML
   in case parsed of
        Left err -> Left err
        Right node -> Right $ fmap parseReportRow (Xeno.children node)

fetchReport :: String -> IO (Either [String] [RowMap])
fetchReport credsPath = do
  creds <- readCredentials credsPath
  case creds of
    Left errs -> return $ Left errs
    Right validCreds -> do
      rpt <- requestReport validCreds
      return $ case parseLSReport rpt of
        Left exception -> Left [show exception]
        Right parsedReport -> Right parsedReport

parseADate :: [Char] -> Maybe C.Day
parseADate dt = parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" dt :: Maybe C.Day

getRowValues :: [String] -> M.Map String (Maybe String) -> [String]
getRowValues colList row =
  let funcsToFindKeysInOrder = map (\k m -> fromMaybe "" (fromMaybe Nothing $ M.lookup k m)) colList
   in funcsToFindKeysInOrder <*> pure row

getRowValues' :: RowMap -> [String]
getRowValues' r = ["Ab", "Bb"]

toCSVStyle :: [RowMap] -> [[String]]
toCSVStyle report =
  let firstRow = head report
      columnNames = M.keys firstRow
      rows = map (\r -> getRowValues columnNames r) report
   in columnNames : rows