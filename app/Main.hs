{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.ByteString.Lazy.Internal (unpackChars)
import Data.Csv (encode)
import Data.List (intercalate)
import LegalServerReader (fetchReport, toCSVStyle)
import System.Console.CmdArgs
  ( Data,
    Default (def),
    Typeable,
    cmdArgs,
    help,
    program,
    summary,
    (&=),
  )

data Opts = Opts {configPath :: String, report :: String, outputFormat :: String} deriving (Show, Data, Typeable)

lsreports =
  Opts
    { configPath = def &= help "Path to api configuration file",
      report = def &= help "Name of the section in the configuration file identifying the report to download",
      outputFormat = "csv" &= help "Format for output (only csv supported for now)"
    }
    &= summary "Client for using the LegalServer Reports API."
    &= program "lsreports"

main :: IO ()
main = do
  args <- cmdArgs lsreports
  let outputType = outputFormat args
  rpt <- fetchReport (configPath args) (report args)
  case rpt of
    Left errs -> print errs
    Right rpt' ->
      case outputType of
        "csv" -> print . toCSVStyle $ rpt'
        _ -> print $ show rpt