{-# LANGUAGE MultiParamTypeClasses #-}
module Class where

import Prelude
import qualified Data.Aeson as A
import Data.Text

class GatewayPFRequestGenerator a b where
    createRequest :: a -> b -> Text -> A.Value -> A.Value