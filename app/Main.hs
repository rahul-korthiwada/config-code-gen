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
import Control.Lens.At
import Data.Aeson
import Prelude
import GHC.List

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
    let validatePayload = validateRequest PAYU "DOTP" (A.object []) (A.object [("txn_s2s_flow" , A.String "02") , ("temp" , A.String "alias") ])
    let validatePayload2 = validateRequest PAYU "TPV" (A.object [("beneficiarydetail", A.String "AccountName")]) (A.object [("temp" , A.String "alias") ])
    let validatePayload3 = validateRequest PAYU "TPV" (A.object [("beneficiarydetail", A.String "AccountName")]) (A.object [("bankDetails" , A.String "Nothing") ])
    let validatePayload4 = validateRequest PAYU "TPV" (A.object [("beneficiarydetail", A.String "AccountName")]) (A.object [("bankDetails" , A.String "AccountName") ])
    putStrLn (show validatePayload)
    putStrLn (show validatePayload2)
    putStrLn (show validatePayload3)
    putStrLn (show validatePayload4)
    pure ()

$(generateGatewayInstances ''PAYU)

-- instance Class.GatewayValidator Main.PAYU where 
--     validateRequest _ flow req gwReq = 
--         let result = case flow of
--                         "DOTP" -> Prelude.and [(Utils.lookupCustom (Control.Lens.At.ix "txn_s2s_flow") req) Prelude.== (Data.Aeson.String "02"),
--                                                     (Utils.lookupCustom (Control.Lens.At.ix "temp") req) Prelude.== (Data.Aeson.String "23")]
--                         _ -> True
--         in result