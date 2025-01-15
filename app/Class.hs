{-# LANGUAGE MultiParamTypeClasses #-}
module Class where

import Prelude
import qualified Data.Aeson as A
import Data.Text


data MandateObject = MandateObject {
    mandate :: Text ,
    frequency :: Text
}

data TPVObject = TPVObject {
    beneficiarydetail :: Text
}

class GatewayPFRequestGenerator a b c where
    createRequest :: a -> b -> c -> Text -> A.Value