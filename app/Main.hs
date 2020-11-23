{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.List (intercalate)
import LegalServerReader (fetchReport, toCSVStyle)
import System.Console.CmdArgs
  ( Data,
    Default (def),
    Typeable,
    cmdArgs,
  )

data Opts = Opts {configPath :: String, outputFormat :: String} deriving (Show, Data, Typeable)

opts = Opts {configPath = def, outputFormat = "csv"}

main :: IO ()
main = do
  args <- cmdArgs opts
  let outputType = outputFormat args
  rpt <- fetchReport (configPath args)
  case rpt of
    Left errs -> print errs
    Right rpt' ->
      case outputType of
        "csv" -> mapM_ putStrLn $ map (intercalate ",") $ toCSVStyle rpt'
        _ -> print $ show rpt