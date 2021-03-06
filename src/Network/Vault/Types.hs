{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

---------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2017 Owain Lewis
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Owain Lewis <owain@owainlewis.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines the core HTTP types dispatched to Vault
---------------------------------------------------------------------------
module Network.Vault.Types
  ( Init(..)
  , VaultSecret(..)
  ) where

import Data.Aeson
import GHC.Generics
import Network.Vault.Internal.Json (vaultJSONOpts)

data Init = Init
    -- Specifies the number of shares to split the master key into.
  { secretShares :: Int
    -- Specifies the number of shares required to reconstruct the master key.
    -- This must be less than or equal secret_shares.
    -- If using Vault HSM with auto-unsealing, this value must be the same as secret_shares.
  , secretThreshold :: Int
    -- Specifies an array of PGP public keys used to encrypt the output unseal keys.
    -- Ordering is preserved. The keys must be base64-encoded from their original binary representation.
    -- The size of this array must be the same as secret_shares.
  , pgpKeys :: Maybe [String]
  } deriving (Eq, Show, Generic)

instance ToJSON Init where
  toJSON = genericToJSON vaultJSONOpts

instance FromJSON Init where
  parseJSON = genericParseJSON vaultJSONOpts

data VaultSecret = VaultSecret {
  value :: String
} deriving ( Eq, Show, Generic )

instance ToJSON VaultSecret where
  toJSON = genericToJSON vaultJSONOpts

instance FromJSON VaultSecret where
  parseJSON = genericParseJSON vaultJSONOpts
