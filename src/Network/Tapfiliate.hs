{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Network.Tapfiliate
  ( API
  , api
  , mkClient
  , tapfiliateAuth
  , TapfiliateClient(..)
  , runTapfiliate
  , getConversion'
  , listConversions'
  , createConversion'
  , addCommissionToConversion'
  , getConversionMetadataCollection'
  , addConversionMetadata'
  , getConversionMetadata'
  , setConversionMetadata'
  , deleteConversionMetadata'
  , getCommission'
  , updateCommission'
  , approveCommission'
  , disapproveCommission'
  , AffiliateId(..)
  , Affiliate(..)
  , Company(..)
  , Address(..)
  , Country(..)
  , getAffiliate'
  , listAffiliates'
  , createAffiliate'
  , getAffiliateMetadataCollection'
  , addAffiliateMetadata'
  -- , getAffiliateMetadata'
  -- , setAffiliateMetadata'
  -- , deleteAffiliateMetadata'
  , ProgramId(..)
  , Program(..)
  , getProgram'
  , listPrograms'
  , addAffiliateToProgram'
  , approveAffiliateForProgram'
  , disapproveAffiliateForProgram'
  , getAffiliateForProgram'
  , getPayout'
  , listPayouts'
  , generatePayouts'
  , markPayoutAsPaid'
  , markPayoutAsUnpaid'
  ) where


import Protolude

import Data.Aeson
import Data.Coerce
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.Time
import Network.Tapfiliate.Internal
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Common.Req

type TapfiliateAuth = AuthenticateReq (AuthProtect "Api-Key")
type instance AuthClientData (AuthProtect "Api-Key") = Text

tapfiliateAuth :: Text -> TapfiliateAuth
tapfiliateAuth t = AuthenticateReq
  (t, \tok req -> req { headers = ("Api-Key", tok) : headers req })

type APIRoute rest = "api" :> "1.4" :> AuthProtect "Api-Key" :> rest
type Wire = '[JSON]

newtype ConversionId = ConversionId Int
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

data Country = Country
  { countryCode :: Text
  , countryName :: Text
  } deriving (Show)

mkJson ''Country

data Address = Address
  { addressAddress :: Text
  , addressAddressTwo :: Maybe Text
  , addressPostalCode :: Text
  , addressCity :: Text
  , addressState :: Maybe Text
  , addressCountry :: Country
  } deriving (Show)

mkJson ''Address

data Company = Company
  { companyName :: Maybe Text
  , companyDescription :: Maybe Text
  , companyAddress :: Address
  } deriving (Show)

mkJson ''Company

newtype AffiliateId = AffiliateId Text
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

data Affiliate = Affiliate
  { affiliateId :: AffiliateId
  , affiliateFirstname :: Text
  , affiliateLastname :: Text
  , affiliateEmail :: Text
  , affiliatePassword :: Maybe Text
  , affiliateCompany :: Maybe Company
  , affiliateMetaData :: Object
  } deriving (Show)

mkJson ''Affiliate

newtype ProgramId = ProgramId Text
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

data Program = Program
  { programId :: ProgramId
  , programTitle :: Text
  , programCurrency :: Text
  , programCookieTime :: Int
  } deriving (Show)

mkJson ''Program

newtype CommissionId = CommissionId Int
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

type GetConversion = APIRoute ("conversions" :> Capture "conversion_id" ConversionId :> Get Wire Object)
type ListConversions = APIRoute
  ("conversions" :>
  QueryParam "program_id" Text :>
  QueryParam "external_id" Text :>
  QueryParam "affiliate_id" AffiliateId :>
  QueryParam "pending" Bool :>
  QueryParam "date_from" Day :>
  QueryParam "date_to" Day :>
  Get Wire [Object])
type CreateConversion = APIRoute
  ("conversions" :>
  QueryParam "override_max_cookie_time" Bool :>
  ReqBody Wire Object :>
  Post Wire Object)
type AddCommissionToConversion = APIRoute
  ("conversions" :>
  Capture "conversion_id" ConversionId :>
  "commissions" :>
  ReqBody Wire Object :>
  Post Wire [Object])

type API =
  GetConversion :<|>
  ListConversions :<|>
  CreateConversion :<|>
  AddCommissionToConversion :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            Get Wire Object) :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            ReqBody Wire Object :>
            PostNoContent '[] NoContent) :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            Capture "key" Text :>
            Get Wire Object) :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            Capture "key" Text :>
            ReqBody Wire Object :>
            PostNoContent '[] NoContent) :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            Capture "key" Text :>
            DeleteNoContent '[] NoContent) :<|>
  APIRoute ("commissions" :>
            Capture "commission_id" CommissionId :>
            Get Wire Object) :<|>
  APIRoute ("commissions" :>
            Capture "commission_id" CommissionId :>
            ReqBody Wire Object :>
            PutNoContent '[] NoContent) :<|>
  APIRoute ("commissions" :>
            Capture "commission_id" CommissionId :>
            "approval" :>
            PutNoContent '[] NoContent) :<|>
  APIRoute ("commissions" :>
            Capture "commission_id" CommissionId :>
            "approval" :>
            DeleteNoContent '[] NoContent) :<|>
  APIRoute ("affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            Get Wire Affiliate) :<|>
  APIRoute ("affiliates" :>
            QueryParam "click_id" Text :>
            QueryParam "source_id" Text :>
            QueryParam "email" Text :>
            Get Wire [Affiliate]) :<|>
  APIRoute ("affiliates" :>
            ReqBody Wire Affiliate :>
            Post Wire Affiliate) :<|>
  APIRoute ("affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            "meta-data" :>
            Get Wire Object) :<|>
  APIRoute ("affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            "meta-data" :>
            ReqBody Wire Object :>
            PutNoContent '[] NoContent) :<|>
  -- Set meta data
  -- Delete meta data
  -- (MLM) Set parent
  -- (MLM) Remove parent
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            Get Wire Program) :<|>
  APIRoute ("programs" :>
            QueryParam "asset_id" Text :>
            Get Wire [Program]) :<|>
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            ReqBody Wire Object :>
            Post Wire Object) :<|>
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            "affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            "approval" :>
            PutNoContent '[] NoContent) :<|>
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            "affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            "approval" :>
            DeleteNoContent '[] NoContent) :<|>
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            "affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            Get Wire Object) :<|>
  APIRoute ("payouts" :>
            Capture "payout_id" Text :>
            Get Wire Object) :<|>
  APIRoute ("payouts" :>
            Get Wire [Object]) :<|>
  APIRoute ("payouts" :>
            ReqBody Wire Object :>
            Post Wire [Object]) :<|>
  APIRoute ("payouts" :>
            Capture "payout_id" Text :>
            "paid" :>
            PutNoContent '[] NoContent) :<|>
  APIRoute ("payouts" :>
            Capture "payout_id" Text :>
            "paid" :>
            DeleteNoContent '[] NoContent)

-- | Value-level representation of API.
api :: Proxy API
api = Proxy

getConversion'
  :: TapfiliateAuth
     -> ConversionId -> ClientM (HashMap Text Value)

listConversions'
  :: TapfiliateAuth
     -> Maybe Text
     -> Maybe Text
     -> Maybe AffiliateId
     -> Maybe Bool
     -> Maybe Day
     -> Maybe Day
     -> ClientM [Object]

createConversion'
  :: TapfiliateAuth
     -> Maybe Bool -> HashMap Text Value -> ClientM (HashMap Text Value)

addCommissionToConversion'
  :: TapfiliateAuth
     -> ConversionId -> HashMap Text Value -> ClientM [Object]

getConversionMetadataCollection'
  :: TapfiliateAuth
     -> ConversionId -> ClientM (HashMap Text Value)

addConversionMetadata'
  :: TapfiliateAuth
     -> ConversionId -> HashMap Text Value -> ClientM NoContent

getConversionMetadata'
  :: TapfiliateAuth
     -> ConversionId -> Text -> ClientM (HashMap Text Value)

setConversionMetadata'
  :: TapfiliateAuth
     -> ConversionId -> Text -> HashMap Text Value -> ClientM NoContent

deleteConversionMetadata'
  :: TapfiliateAuth
     -> ConversionId -> Text -> ClientM NoContent

getCommission'
  :: TapfiliateAuth
     -> Int -> ClientM (HashMap Text Value)

updateCommission'
  :: TapfiliateAuth
     -> Int -> HashMap Text Value -> ClientM NoContent

approveCommission'
  :: TapfiliateAuth
     -> Int -> ClientM NoContent

disapproveCommission'
  :: TapfiliateAuth
     -> Int -> ClientM NoContent

getAffiliate' ::
  TapfiliateAuth
  -> AffiliateId -> ClientM Affiliate

listAffiliates'
  :: TapfiliateAuth
     -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM [Affiliate]

createAffiliate'
  :: TapfiliateAuth
     -> Affiliate -> ClientM Affiliate

getAffiliateMetadataCollection' ::
  TapfiliateAuth
  -> AffiliateId -> ClientM (HashMap Text Value)

addAffiliateMetadata' ::
  TapfiliateAuth
  -> AffiliateId -> HashMap Text Value -> ClientM NoContent

getProgram' ::
  TapfiliateAuth
  -> ProgramId -> ClientM Program

listPrograms' ::
  TapfiliateAuth
  -> Maybe Text -> ClientM [Program]

addAffiliateToProgram' ::
  TapfiliateAuth
  -> ProgramId -> HashMap Text Value -> ClientM (HashMap Text Value)

approveAffiliateForProgram' ::
  TapfiliateAuth
  -> ProgramId -> AffiliateId -> ClientM NoContent

disapproveAffiliateForProgram' ::
  TapfiliateAuth
  -> ProgramId -> AffiliateId -> ClientM NoContent

getAffiliateForProgram' ::
  TapfiliateAuth
  -> ProgramId -> AffiliateId -> ClientM (HashMap Text Value)

getPayout' ::
  TapfiliateAuth
  -> Text -> ClientM (HashMap Text Value)

listPayouts' ::
  TapfiliateAuth -> ClientM [Object]

generatePayouts' ::
  TapfiliateAuth
  -> HashMap Text Value -> ClientM [Object]

markPayoutAsPaid' ::
  TapfiliateAuth
  -> Text -> ClientM NoContent

markPayoutAsUnpaid' ::
  TapfiliateAuth
  -> Text -> ClientM NoContent

(getConversion' :<|>
 listConversions' :<|>
 createConversion' :<|>
 addCommissionToConversion' :<|>
 getConversionMetadataCollection' :<|>
 addConversionMetadata' :<|>
 getConversionMetadata' :<|>
 setConversionMetadata' :<|>
 deleteConversionMetadata' :<|>
 getCommission' :<|>
 updateCommission' :<|>
 approveCommission' :<|>
 disapproveCommission' :<|>
 getAffiliate' :<|>
 listAffiliates' :<|>
 createAffiliate' :<|>
 getAffiliateMetadataCollection' :<|>
 addAffiliateMetadata' :<|>
 -- getAffiliateMetadata' :<|>
 -- setAffiliateMetadata' :<|>
 -- deleteAffiliateMetadata' :<|>
 getProgram' :<|>
 listPrograms' :<|>
 addAffiliateToProgram' :<|>
 approveAffiliateForProgram' :<|>
 disapproveAffiliateForProgram' :<|>
 getAffiliateForProgram' :<|>
 getPayout' :<|>
 listPayouts' :<|>
 generatePayouts' :<|>
 markPayoutAsPaid' :<|>
 markPayoutAsUnpaid') = client api

newtype TapfiliateClient = TapfiliateClient { fromTapfiliateClient :: ClientEnv }

runTapfiliate :: TapfiliateClient -> TapfiliateM a -> TapfiliateResponse a
runTapfiliate c m = runClientM m (coerce c)

type TapfiliateM = ClientM
type TapfiliateResponse r = IO (Either ServantError r)

mkClient :: IO TapfiliateClient
mkClient = do
  man <- getGlobalManager
  u <- parseBaseUrl "https://tapfiliate.com"
  return $ TapfiliateClient $ ClientEnv man u
