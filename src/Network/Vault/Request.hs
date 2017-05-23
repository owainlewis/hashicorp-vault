{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Owain Lewis
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the basic HTTP request plubming
--------------------------------------------------------------------------
module Network.Vault.Request
  ( vaultRequest
  , vaultRequestJSON
  ) where

import Control.Exception
import Control.Exception (throwIO)
import Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (statusCode)

import qualified Data.Aeson as Aeson

data VaultRequestException =
  JSONDecodeException String
  deriving (Show, Eq)

instance Exception VaultRequestException

buildFullRequestPath :: String -> String -> String
buildFullRequestPath vaultEndpoint requestPath =
  vaultEndpoint ++ "/v1" ++ requestPath

vaultRequest
  :: Aeson.ToJSON a
  => Manager
  -> String
  -> Method
  -> String
  -> Maybe a
  -> [Header]
  -> IO ByteString
vaultRequest manager vaultEndpoint rMethod rPath rBody rHeaders = do
  initialRequest <- parseRequest (buildFullRequestPath vaultEndpoint rPath)
  let reqBody = fromMaybe LBS.empty (Aeson.encode <$> rBody)
      req =
        initialRequest
        { method = rMethod
        , requestBody = RequestBodyLBS reqBody
        , requestHeaders = [("Content-Type", "application/json")] ++ rHeaders
        }
  response <- httpLbs req manager
  pure (responseBody response)

vaultRequestJSON
  :: (Aeson.ToJSON a, Aeson.FromJSON b)
  => Manager -> String -> Method -> String -> Maybe a -> [Header] -> IO b
vaultRequestJSON manager vaultEndpoint rMethod rPath rBody rHeaders = do
  responseBody <-
    vaultRequest manager vaultEndpoint rMethod rPath rBody rHeaders
  case Aeson.eitherDecode responseBody of
    Left err -> throwIO $ JSONDecodeException err
    Right x -> pure x
