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
  , VaultErrors
  ) where

import Control.Exception
import Control.Exception (throwIO)
import Control.Monad (mzero)
import Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (statusCode)

import Data.Aeson

data VaultRequestException =
  JSONDecodeException String
  deriving (Show, Eq)

instance Exception VaultRequestException

data VaultErrors = VaultErrors
  { errors :: [String]
  } deriving (Show)

instance FromJSON VaultErrors where
  parseJSON (Object v) = VaultErrors <$> v .: "errors"
  parseJSON _ = mzero

data VaultResponse a
  = VaultSuccess a
  | VaultFailure Int
                 VaultErrors
  deriving (Show, Generic)

buildFullRequestPath :: String -> String -> String
buildFullRequestPath vaultEndpoint requestPath =
  vaultEndpoint ++ "/v1" ++ requestPath

-- | Makes a HTTP request to the Vault API. If a non 200 status is returned we can
--   automatically decode into a known error format since the Vault API is
--   consistent
vaultRequest
  :: ToJSON a
  => Manager
  -> String
  -> Method
  -> String
  -> Maybe a
  -> [Header]
  -> IO (VaultResponse LBS.ByteString)
vaultRequest manager vaultEndpoint rMethod rPath rBody rHeaders = do
  initialRequest <- parseRequest (buildFullRequestPath vaultEndpoint rPath)
  let reqBody = fromMaybe LBS.empty (encode <$> rBody)
      req =
        initialRequest
        { method = rMethod
        , requestBody = RequestBodyLBS reqBody
        , requestHeaders = [("Content-Type", "application/json")] ++ rHeaders
        }
  response <- httpLbs req manager
  let status = statusCode (responseStatus response)
      body = responseBody response
  if status == 200 || status == 201
    then return $ VaultSuccess body
    else case decode body of
           Just errs -> pure (VaultFailure status errs)
           Nothing ->
             throwIO $
             JSONDecodeException
               "Unexpected API response from Vault. Could not decode"

-- | vaultRequestJSON allows a user to post in a type as the request body.
--   The body type must derive a ToJSON instance and the response you want must
--   derive a FromJSON instance
vaultRequestJSON
  :: (ToJSON a, FromJSON b)
  => Manager
  -> String
  -> Method
  -> String
  -> Maybe a
  -> [Header]
  -> IO (VaultResponse b)
vaultRequestJSON manager vaultEndpoint rMethod rPath rBody rHeaders = do
  response <- vaultRequest manager vaultEndpoint rMethod rPath rBody rHeaders
  case response of
    VaultSuccess lbs ->
      case eitherDecode lbs of
        Left err -> throwIO $ JSONDecodeException err
        Right x -> pure (VaultSuccess x)
    vf@(VaultFailure s e) -> pure (VaultFailure s e)
