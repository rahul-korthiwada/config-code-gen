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
import qualified Data.Set as DS

type NestedMap = Map String NestedMap

-- Function to insert a dotted path into the nested map
insertPath :: [String] -> NestedMap -> NestedMap
insertPath [] m = m
insertPath (x:xs) m =
  let subMap = fromMaybe Map.empty (Map.lookup x m)
  in Map.insert x (insertPath xs subMap) m

-- Convert a list of dotted strings into a nested map
buildNestedMap :: [String] -> NestedMap
buildNestedMap = foldl' (flip (insertPath . splitDots)) Map.empty

-- Split a dotted string into its components
splitDots :: String -> [String]
splitDots = words . map replaceDot
  where
    replaceDot '.' = ' '
    replaceDot c   = c

-- Main function to transform input to output
transform :: [String] -> String
transform input =
  let nestedMap = buildNestedMap input
  in serializeNestedMap nestedMap

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

generateGatewayInstances :: Name -> Name -> Q [Dec]
generateGatewayInstances gw genericRequest = do
    contents <- getFileContents gw
    runIO $ print contents
    validateGenericRequestType genericRequest contents
    validateFuncDec <- generateValidateRequestFunc contents
    return [InstanceD Nothing [] (AppT (AppT (ConT ''GatewayPFRequestGenerator) (ConT gw)) (ConT genericRequest)) [validateFuncDec]]
    -- return [InstanceD Nothing [] (AppT (ConT ''KVConnector) (AppT (ConT name) (ConT $ mkName "Identity"))) [tableNameD, keyMapD, primaryKeyD, secondaryKeysD, getCreateCounterKeyD]]

validateGenericRequestType :: Name -> PaymentFlowRules -> Q ()
validateGenericRequestType genericRequest pfRules = do
    dataType <- reify genericRequest
    let dataKeys = DS.fromList $ extractKeys dataType 
        rules = (toList . paymentflowrules) pfRules
    let specificPayloadKeys = DS.fromList $ Prelude.concat (Prelude.map (\( _ , rule) -> Prelude.map (unpack . UtilsTH.key) rule) rules)
        commonKeys = DS.intersection dataKeys specificPayloadKeys
    if Prelude.length commonKeys == 0 
        then pure () 
        else fail ("Have Duplicate Keys in Generic and Specific Gateway Payloads, Please move common feilds to flow specific , Fields :: " <> (show commonKeys))
    where
        extractKeys (TyConI (DataD _ _ _ _ payload _)) =  Prelude.concat (Prelude.map extractKeys' payload)
        extractKeys _                             = []    

        extractKeys' (RecC _ payload)              =  Prelude.map ((\(name,_,_) -> nameBase name)) payload
        extractKeys' _ = []
        

generateValidateRequestFunc :: PaymentFlowRules -> Q Dec
generateValidateRequestFunc pfRules = do
    let name = mkName "createRequest"
        genericPayload = mkName "genericReq"
        reqName = mkName "req"
        flowName = mkName "flow"
        resultName =  mkName "result"
        rules = (toList . paymentflowrules) pfRules
        wildCardMatch = Match WildP (NormalB (ConE 'Null)) []
        matchLiterals = Prelude.map (generateMatchLiteral reqName) rules <> [wildCardMatch]
        caseBody = NormalB (CaseE (VarE flowName) matchLiterals)
        body = NormalB (LetE [ValD (VarP resultName)  caseBody []] (VarE resultName) )
    return ( FunD name [Clause [WildP, VarP genericPayload, VarP flowName, VarP reqName] body []])

generateMatchLiteral :: Name -> (Text, [Mappings]) -> Match
generateMatchLiteral reqName (flow, mapppings ) = do
    let matchL = LitP (StringL (unpack flow))
        matchBody = AppE (VarE 'object) (ListE (Prelude.map generateBody mapppings))
        matchLiteral = Match matchL (NormalB matchBody) []
    matchLiteral
    where
        generateBody mapping = do
            let keyLit = LitE (StringL (unpack (UtilsTH.key mapping)))
                valueLit = case operator mapping of
                                UtilsTH.EQ -> AppE (ConE 'String) (LitE (StringL (unpack (value mapping))))
                                MAP ->
                                    let valueInSplits = unpack <$> Data.Text.split (=='.') (value mapping)
                                        lookUpExpr = Prelude.foldl (\acc exp -> InfixE (Just acc) (VarE '(.)) (Just (InfixE (Just (VarE '_Value)) (VarE '(.)) (Just (generateIxExpr exp))))) (generateIxExpr (Prelude.head valueInSplits)) (Prelude.tail valueInSplits)
                                    in
                                        AppE (AppE (VarE 'lookupCustom) lookUpExpr) (VarE reqName)
            TupE [Just keyLit, Just valueLit]

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