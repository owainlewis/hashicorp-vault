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
import Network.Vault.Request (vaultRequestJSON, vaultRequest, VaultResponse(..))

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
  => VaultConfig -> Method -> String -> Maybe a -> [Header] -> IO (VaultResponse LBS.ByteString)
vaultConfigRequest config =
  let (VaultConfig endpoint handler) = config
  in vaultRequest handler endpoint

-- vaultConfigRequestWithToken
--   :: (ToJSON a, FromJSON b)
--   => VaultConfig -> VaultAuthToken -> Method -> String -> Maybe a -> [Header] -> IO b
-- vaultConfigRequestWithToken (VaultConfig endpoint handler) vaultAuthToken rMethod rPath rBody rHeaders =
--   vaultRequestJSON handler endpoint rMethod rPath rBody headers
--   where
--     headers = [("X-Vault-Token", token vaultAuthToken)] ++ rHeaders

---------------------------------------------------

exampleInit = Init 1 1 Nothing

--vaultInit config init = vaultConfigRequest config "PUT" "/" (Just init) []

xample init = do
  config <- mkDefaultVaultConfig "http://localhost:8200"
  vaultConfigRequest config "PUT" "/sys/init" (Just init) []
