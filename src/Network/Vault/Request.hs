{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Owain Lewis
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the basic HTTP request plubming
----------------------------------------------------------------------------
module Network.Vault.Request where

-- Remove this
import Control.Exception
--

import Control.Exception (throwIO)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.ByteString.Lazy as LBS
import Data.Maybe(fromMaybe)
import Data.Semigroup((<>))

import qualified Data.Aeson as Aeson

data VaultConfiguration = VaultConfiguration {
    vaultEndpoint :: String
} deriving ( Eq, Show )

data VaultException
    = JSONDecodeException String
    deriving (Show, Eq)

instance Exception VaultException

defaultHeaders = [ ("Content-Type", "application/json") ]

buildFullRequestPath :: VaultConfiguration -> String -> String
buildFullRequestPath vaultConfiguration requestPath =
    (vaultEndpoint vaultConfiguration) ++ "/v1" ++ requestPath

vaultRequest
  :: Aeson.ToJSON a =>
     Manager
     -> VaultConfiguration
     -> Method
     -> String
     -> Maybe a
     -> [Header]
     -> IO ByteString
vaultRequest manager vaultConfiguration rMethod rPath rBody rHeaders = do
    initialRequest <-
        parseRequest (buildFullRequestPath vaultConfiguration rPath)
    let reqBody = fromMaybe LBS.empty (Aeson.encode <$> rBody)
        req = initialRequest
            { method = rMethod
            , requestBody = RequestBodyLBS reqBody
            , requestHeaders = rHeaders
            }
    response <- httpLbs req manager
    pure (responseBody response)

vaultRequestJSON
  :: (Aeson.ToJSON a, Aeson.FromJSON b) =>
     Manager
     -> VaultConfiguration
     -> Method
     -> String
     -> Maybe a
     -> [Header]
     -> IO b
vaultRequestJSON manager vaultConfiguration rMethod rPath rBody rHeaders = do
    responseBody <-
        vaultRequest manager vaultConfiguration rMethod rPath rBody rHeaders
    case Aeson.eitherDecode responseBody of
        Left err -> throwIO $ JSONDecodeException err
        Right x -> pure x
