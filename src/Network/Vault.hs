{-# LANGUAGE OverloadedStrings #-}

module Network.Vault where

import qualified Data.ByteString.Lazy as LBS

data ApiVersion = V1 deriving ( Eq )

instance Show ApiVersion where
    show (V1) = "v1"

data VaultConfig = VaultConfig {
    endpoint :: String
  , version :: ApiVersion
} deriving ( Eq, Show )

data VaultResponse a = Success a
                     | Errors [String]
                     deriving ( Show )
---------------

localConfig = VaultConfig { endpoint = "http://127.0.0.1:8200"
                          , version = V1
                          }
