module Utils where

import Prelude
import Control.Lens
import qualified Data.Aeson as A

-- We can optimise the given text lookup into more advanced Lens Lookup
-- lookupCustom :: (IxValue A.Value) -> A.Value -> A.Value
lookupCustom key value = 
    case value ^? key of
        Just res -> res
        Nothing -> A.Null
    