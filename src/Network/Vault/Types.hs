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
-- This module defines the basic HTTP request plubming
---------------------------------------------------------------------------
module Network.Vault.Types where

import Data.Aeson
import GHC.Generics

import Data.Aeson.Casing(aesonPrefix, snakeCase)

data Init = Init {
    initSecretShares :: Int
  , initSecretThreshold :: Int
} deriving (Show, Generic)

instance ToJSON Init where
     toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON Init where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

example = encode $ Init 10 20
