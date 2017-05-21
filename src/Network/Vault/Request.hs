{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Network.Vault.Request where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Data.Aeson
import GHC.Generics

data InitRequest = InitRequest {
    secretShares :: Int
  , secretThreshold :: Int
} deriving (Generic)

instance ToJSON InitRequest where
     toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON InitRequest where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

--main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://127.0.0.1:8200/v1/sys/init"
    let request = initialRequest {
            method = "GET"
--          , requestBody = RequestBodyLBS $ encode requestObject
--          , requestHeaders = [("foo", "bar")]
          }
    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response


