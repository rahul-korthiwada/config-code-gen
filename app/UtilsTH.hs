{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TemplateHaskell, OverloadedStrings #-}

module UtilsTH where

import Prelude
import qualified Data.Char as DC
import Language.Haskell.TH
import Data.Text
import Data.Aeson
import qualified Data.List.Split as DLS (splitOn)
import qualified Data.List as DL
import GHC.Generics
import Data.Yaml (decodeFileEither, ParseException)
import Data.Map
import Class
import Utils
import Control.Lens
import Language.Haskell.TH (Exp(AppE))
import Data.Aeson.Lens

newtype PaymentFlowRules = PaymentFlowRules {
    paymentflowrules :: Map Text [Mappings]
} deriving (Generic, Show, FromJSON, ToJSON)

data Mappings = Mappings {
    key :: Text,
    operator :: Operator,
    arguments :: Maybe Text,
    value :: Text
} deriving (Generic, Show, FromJSON, ToJSON)

data Operator = EQ | MAP
    deriving (Generic, Show, FromJSON, ToJSON)

toLowerCase :: String -> String
toLowerCase = DL.map DC.toLower

generateGatewayInstances :: Name -> Q [Dec]
generateGatewayInstances gw = do
    contents <- getFileContents gw
    runIO $ print contents
    validateFuncDec <- generateValidateRequestFunc contents

    return [InstanceD Nothing [] (AppT (ConT ''GatewayValidator) (ConT gw)) [validateFuncDec]]
    -- return [InstanceD Nothing [] (AppT (ConT ''KVConnector) (AppT (ConT name) (ConT $ mkName "Identity"))) [tableNameD, keyMapD, primaryKeyD, secondaryKeysD, getCreateCounterKeyD]]

-- generateValidateRequestFunc :: PaymentFlowRules -> Q Dec
-- generateValidateRequestFunc pfRules = do
--     let name = mkName "validateRequest"
--         reqName = mkName "req"
--         flowName = mkName "flow"
--         resultName =  mkName "result"
--         rules = (toList . paymentflowrules) pfRules
--         wildCardMatch = Match WildP (NormalB (ConE 'Null)) []
--         matchLiterals = Prelude.map (generateMatchLiteral reqName) rules <> [wildCardMatch]
--         caseBody = NormalB (CaseE (VarE flowName) matchLiterals)
--         body = NormalB (LetE [ValD (VarP resultName)  caseBody []] (VarE resultName) )
--     return ( FunD name [Clause [WildP, VarP flowName, VarP reqName] body []])

-- generateMatchLiteral :: Name -> (Text, [Mappings]) -> Match
-- generateMatchLiteral reqName (flow, mapppings ) = do
--     let matchL = LitP (StringL (unpack flow))
--         matchBody = AppE (VarE 'object) (ListE (Prelude.map generateBody mapppings))
--         matchLiteral = Match matchL (NormalB matchBody) []
--     matchLiteral
--     where
--         generateBody mapping = do
--             let keyLit = LitE (StringL (unpack (UtilsTH.key mapping)))
--                 valueLit = case operator mapping of
--                                 UtilsTH.EQ -> AppE (ConE 'String) (LitE (StringL (unpack (value mapping))))
--                                 MAP ->
--                                     let valueInSplits = unpack <$> Data.Text.split (=='.') (value mapping)
--                                         lookUpExpr = Prelude.foldl (\acc exp -> InfixE (Just acc) (VarE '(.)) (Just (InfixE (Just (VarE '_Value)) (VarE '(.)) (Just (generateIxExpr exp))))) (generateIxExpr (Prelude.head valueInSplits)) (Prelude.tail valueInSplits)
--                                     in
--                                         AppE (AppE (VarE 'lookupCustom) lookUpExpr) (VarE reqName)
--             TupE [Just keyLit, Just valueLit]

--         generateIxExpr lK =
--             let lKE = (LitE (StringL lK))
--             in
--                 ( AppE (VarE 'ix) lKE )

generateValidateRequestFunc :: PaymentFlowRules -> Q Dec
generateValidateRequestFunc pfRules = do
    let name = mkName "validateRequest"
        reqName = mkName "req"
        flowName = mkName "flow"
        gwReqName =  mkName "gwReq"
        resultName =  mkName "result"
        rules = (toList . paymentflowrules) pfRules
        wildCardMatch = Match WildP (NormalB (ConE 'True)) []
        matchLiterals = Prelude.map (generateMatchLiteral reqName gwReqName) rules <> [wildCardMatch]
        caseBody = NormalB (CaseE (VarE flowName) matchLiterals)
        body = NormalB (LetE [ValD (VarP resultName)  caseBody []] (VarE resultName) )
    return ( FunD name [Clause [WildP, VarP flowName, VarP reqName, VarP gwReqName] body []])

generateMatchLiteral :: Name -> Name -> (Text, [Mappings]) -> Match
generateMatchLiteral reqName gwReqName (flow, mapppings ) = do
    let matchL = LitP (StringL (unpack flow))
        matchBody = AppE (VarE 'Prelude.and) (ListE (Prelude.map generateBody mapppings))
        matchLiteral = Match matchL (NormalB matchBody) []
    matchLiteral
    where
        generateBody mapping = do
            let keyLit = generateKeyLit mapping
                    
            let valueLit = case operator mapping of
                                UtilsTH.EQ -> AppE (ConE 'String) (LitE (StringL (unpack (value mapping))))
                                MAP ->
                                    let valueInSplits = unpack <$> Data.Text.split (=='.') (value mapping)
                                        lookUpExpr = Prelude.foldl (\acc exp -> InfixE (Just acc) (VarE '(.)) (Just (InfixE (Just (VarE '_Value)) (VarE '(.)) (Just (generateIxExpr exp))))) (generateIxExpr (Prelude.head valueInSplits)) (Prelude.tail valueInSplits)
                                    in
                                        AppE (AppE (VarE 'lookupCustom) lookUpExpr) (VarE reqName)
            InfixE (Just (ParensE (keyLit))) (VarE '(==)) (Just (ParensE (valueLit)))

        generateKeyLit mapping = do
            let valueInSplits = unpack <$> Data.Text.split (=='.') (UtilsTH.key mapping)
                lookUpExpr = Prelude.foldl (\acc exp -> InfixE (Just acc) (VarE '(.)) (Just (InfixE (Just (VarE '_Value)) (VarE '(.)) (Just (generateIxExpr exp))))) (generateIxExpr (Prelude.head valueInSplits)) (Prelude.tail valueInSplits)
            AppE (AppE (VarE 'lookupCustom) lookUpExpr) (VarE gwReqName)

        generateIxExpr lK =
            let lKE = (LitE (StringL lK))
            in
                ( AppE (VarE 'ix) lKE )


getFileContents :: Name -> Q PaymentFlowRules
getFileContents gw = do
    let gwInString = toLowerCase $ DL.last $ DLS.splitOn "." (show gw)
    eRules <- runIO $ decodeFileEither ("./app/" <>  gwInString <> "Configuration.yaml")
    case eRules of
        Right rules -> pure rules
        Left  err -> fail ("GatewayValidatorException :: Unable to Parse Rule File :: Error " <> show err)