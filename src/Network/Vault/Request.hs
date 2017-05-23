{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
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
  , VaultResponse(..)
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

import GHC.Generics

import qualified Data.Aeson as Aeson

data VaultRequestException =
    JSONDecodeException String
    deriving (Show, Eq)

instance Exception VaultRequestException

data VaultError = VaultError { errors :: [String] }
    deriving ( Show, Generic )

data VaultResponse a = Success a
                     | Failure VaultError
                     deriving ( Show, Generic )

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
  -> IO (VaultResponse LBS.ByteString)
vaultRequest manager vaultEndpoint rMethod rPath rBody rHeaders = do
  initialRequest <- parseRequest (buildFullRequestPath vaultEndpoint rPath)
  let reqBody = fromMaybe LBS.empty (Aeson.encode <$> rBody)
      req =
        initialRequest
        { method = rMethod
        , requestBody = RequestBodyLBS reqBody
        , requestHeaders = [("Content-Type", "application/json")] ++ rHeaders
        }
  print reqBody
  response <- httpLbs req manager
  let status = statusCode (responseStatus response)
  if status == 200 || status == 201 then
    return $ Success (responseBody response)
  else
      case (Aeson.eitherDecode (responseBody response)) of
        Left err -> throwIO $ JSONDecodeException err
        Right x -> return $ Failure x

vaultRequestJSON = id
-- vaultRequestJSON
--   :: (Aeson.ToJSON a, Aeson.FromJSON b)
--   => Manager -> String -> Method -> String -> Maybe a -> [Header] -> IO b
-- vaultRequestJSON manager vaultEndpoint rMethod rPath rBody rHeaders = do
--   responseBody <-
--     vaultRequest manager vaultEndpoint rMethod rPath rBody rHeaders
--   case Aeson.eitherDecode responseBody of
--     Left err -> throwIO $ JSONDecodeException err
--     Right x -> pure x
