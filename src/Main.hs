{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Simple ( httpBS, getResponseBody )
import Control.Lens ( preview )
import Data.Aeson.Lens ( key, _String )
import qualified Data.ByteString.Char8 as BS
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

fetchJSON :: IO BS.ByteString
fetchJSON = do
  res <- httpBS "https://api.coindesk.com/v1/bpi/currentprice.json"
  return (getResponseBody res)

getRate :: Text -> BS.ByteString -> Maybe Text
getRate currency = preview (key "bpi" . key currency . key "rate" . _String)

main :: IO ()
main = do
  json <- fetchJSON
  putStrLn "Choose a currency: USD, GBP, or EUR"
  currency <- getLine
  case getRate (T.toUpper (T.pack currency)) json of
    Nothing -> TIO.putStrLn $ "Could not find the Bitcoin rate in " <> (T.toUpper (T.pack currency))
    Just rate -> TIO.putStrLn $ "The current price of Bitcoin in " <> (T.toUpper (T.pack currency)) <> " is " <> rate
