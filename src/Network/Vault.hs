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
module Network.Vault where

import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.List(isPrefixOf)

type VaultToken = String

data VaultConfig = VaultConfig {
    vaultEndpoint :: String
  , vaultToken :: VaultToken
  , vaultHTTPManager :: Manager
}

mkDefaultVaultConfig :: String -> VaultToken -> IO VaultConfig
mkDefaultVaultConfig endpoint token = do
    let ioManager = if "https" `isPrefixOf` "https://foo.com"
          then newManager tlsManagerSettings
          else newManager defaultManagerSettings
    manager <- ioManager
    pure $ VaultConfig endpoint token manager
