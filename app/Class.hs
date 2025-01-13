module Class where

import Prelude
import qualified Data.Aeson as A
import Data.Text

class GatewayValidator a where
    validateRequest :: a -> Text -> A.Value -> A.Value -> Bool