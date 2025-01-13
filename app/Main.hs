{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Prelude
import UtilsTH
import Class 
import Data.Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Data.Aeson as A
import Control.Lens
import Utils
import Data.Aeson.Lens

data BankDetail = BankDetail {
    bankAccountNumber :: Text,
    bankAccountName :: Text
}

data PAYU = PAYU

data GenericPayload = GenericPayload {
    bankDetail :: BankDetail,
    amount :: Text 
}

-- add :: Int -> Int -> Int
add x y = x + y

-- inspectFunction :: Q [Dec]
-- inspectFunction = do
--     info <- reifyDec 'add
--     runIO (print info)  -- Use runIO to print info at compile-time
--     return []


main :: IO ()
main = do
    let validatePayload = validateRequest PAYU "DOTP" (A.object [("bankDetails" , A.String "Common") ])
    putStrLn (show validatePayload)

-- $(do
--     dec <- [d| k req = lookupCustom (ix "res_code"._Value. ix "res_number" ) req
--             |]
--     runIO (print dec)
--     return [])

$(generateGatewayInstances ''PAYU)