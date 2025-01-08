{-# LANGUAGE OverloadedStrings #-}

module Inputs where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (SomeException, try)
import Control.Lens
import Data.Aeson (object, (.=))
import Data.Aeson.Lens
  ( AsNumber (_Integer),
    AsValue (_String),
    key,
  )
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.Wreq
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import Text.Printf (printf)

data InputType = Actual | Sample

aocYear :: Int
aocYear = 2025

getFileName :: InputType -> Int -> String
getFileName inputType dayNum = case inputType of
  Actual -> printf "inputs/actual/%02d.txt" dayNum
  Sample -> printf "inputs/sample/%02d.txt" dayNum

readInput :: InputType -> Int -> IO (Maybe String)
readInput inputType dayNum = do
  let filename = getFileName inputType dayNum

  fileExists <- doesFileExist filename
  if fileExists
    then Just <$> readFile filename
    else pure Nothing

downloadInput :: Int -> IO ()
downloadInput dayNum = do
  loadFile defaultConfig
  cookie <- B.pack <$> getEnv "AOC_COOKIE"
  let opts = defaults & header "Cookie" .~ [cookie]
      uri = printf "https://adventofcode.com/%d/day/%d/input" aocYear dayNum

  res <- try $ getWith opts uri

  case res of
    Left err -> do
      case err of
        HttpExceptionRequest _ (StatusCodeException resp _) -> do
          let status = resp ^. responseStatus
          case status ^. statusCode of
            404 -> putStrLn "Not found (requesting too early?)"
            x -> do
              let msg = status ^. statusMessage
              putStrLn $ printf "Error submitting answer: HTTP %d [%s]" x $ B.unpack msg
        _ -> putStrLn $ "Unknown error: " ++ show err
    Right res -> do
      writeFile (getFileName Sample dayNum) ""
      BL.writeFile (getFileName Actual dayNum) (res ^. responseBody)
