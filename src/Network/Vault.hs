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
-- A Haskell interface for working with Hashicorp Vault
--
----------------------------------------------------------------------------
module Network.Vault
  ( VaultAuthToken
  , VaultConfig
  , mkDefaultVaultConfig
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.List (isPrefixOf)
import Network.HTTP.Client
       (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Vault.Request
       (vaultRequestJSON, vaultRequest, VaultResponse(..))

import Network.Vault.Types

import qualified Data.ByteString.Lazy as LBS

newtype VaultAuthToken = VaultAuthToken
  { token :: ByteString
  } deriving (Show)

data VaultConfig = VaultConfig
  { vaultEndpoint :: String
  , vaultHTTPManager :: Manager
  }

-- | Smart constructor that enables the correct http manager defaults
--   for a given vault HTTP endpoint
--
--   > mkDefaultVaultConfig "http://localhost:8080"
--
mkDefaultVaultConfig :: String -> IO VaultConfig
mkDefaultVaultConfig endpoint = do
  let ioManager =
        if "https" `isPrefixOf` endpoint
          then newManager tlsManagerSettings
          else newManager defaultManagerSettings
  manager <- ioManager
  pure $ VaultConfig endpoint manager

vaultConfigRequest
  :: (ToJSON a)
  => VaultConfig
  -> Method
  -> String
  -> Maybe a
  -> [Header]
  -> IO (VaultResponse LBS.ByteString)
vaultConfigRequest (VaultConfig endpoint handler) = vaultRequest handler endpoint

vaultConfigRequestWithToken
  :: ToJSON a =>
     VaultConfig
     -> VaultAuthToken
     -> Method
     -> String
     -> Maybe a
     -> [Header]
     -> IO (VaultResponse LBS.ByteString)
vaultConfigRequestWithToken config vaultAuthToken rMethod rPath rBody rHeaders =
  vaultConfigRequest config rMethod rPath rBody headers
  where
    headers = [("X-Vault-Token", token vaultAuthToken)] ++ rHeaders
---------------------------------------------------

vaultInit :: Init -> IO (VaultResponse LBS.ByteString)
vaultInit init = do
  config <- mkDefaultVaultConfig "http://localhost:8200"
  vaultConfigRequest config "PUT" "/sys/init" (Just init) []

createSecret vaultAuthToken k secret = do
  config <- mkDefaultVaultConfig "http://localhost:8200"
  vaultConfigRequestWithToken config vaultAuthToken "POST" ("/secrets/foo") (Just secret) []

sample = createSecret (VaultAuthToken "18516117-2a36-f165-d40d-58b87ba8278e") "foo" (VaultSecret { value = "bar" })
