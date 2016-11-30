module Network.Tapfiliate.Internal
  ( mkJson
  ) where

import Data.Aeson.TH
import Language.Haskell.TH
import Prelude

mkJson :: Name -> Q [Dec]
mkJson n = deriveJSON (defaultOptions { fieldLabelModifier = modifier }) n
  where
    modifier = drop (length $ nameBase n)
