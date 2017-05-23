module Network.Vault.Internal.Json
  ( vaultJSONOpts
  ) where

import Data.Aeson.Types
       (defaultOptions, omitNothingFields, fieldLabelModifier, Options)
import Data.Char (isUpper, toLower)

-- | Default JSON options for Vault
vaultJSONOpts :: Options
vaultJSONOpts =
  defaultOptions {omitNothingFields = True, fieldLabelModifier = snakeCase}

snakeCase :: String -> String
snakeCase [] = []
snakeCase (x:xs)
  | isUpper x = '_' : toLower x : snakeCase xs
  | otherwise = x : snakeCase xs
