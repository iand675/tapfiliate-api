module Network.Tapfiliate.Internal
  ( mkJson
  ) where

import Data.Aeson.TH
import Data.Aeson.Types
import Language.Haskell.TH
import Prelude

mkJson :: Name -> Q [Dec]
mkJson n = deriveJSON (defaultOptions { fieldLabelModifier = modifier }) n
  where
    modifier = camelTo2 '_' . drop (length $ nameBase n)
