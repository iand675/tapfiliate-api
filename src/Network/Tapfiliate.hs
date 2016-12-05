{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Network.Tapfiliate
  ( API
  , api
  , mkClient
  , getEnvAuth
  , tapfiliateAuth
  , TapfiliateClient(..)
  , TapfiliateM
  , runTapfiliate
  , NewConversion(..)
  , NewClick(..)
  , Conversion(..)
  , Click(..)
  , ConversionCommission(..)
  , getConversion'
  , listConversions'
  , createConversion'
  , addCommissionToConversion'
  , getConversionMetadataCollection'
  , addConversionMetadata'
  , getConversionMetadata'
  , setConversionMetadata'
  , deleteConversionMetadata'
  , CommissionId(..)
  , Commission(..)
  , OverrideCommission(..)
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
import qualified Data.Text as T
import Data.Time
import Network.Tapfiliate.Internal
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Servant.Common.Req
import System.Environment

data TrailingSlash

instance HasClient api => HasClient (TrailingSlash :> api) where
  type Client (TrailingSlash :> api) = Client api
  clientWithRoute Proxy req = clientWithRoute (Proxy :: Proxy api) (appendToPath "" req)

getEnvAuth :: IO TapfiliateAuth
getEnvAuth =
  (tapfiliateAuth . T.pack) <$> getEnv "TAPFILIATE_KEY"

type TapfiliateAuth = AuthenticateReq (AuthProtect "Api-Key")
type instance AuthClientData (AuthProtect "Api-Key") = Text

tapfiliateAuth :: Text -> TapfiliateAuth
tapfiliateAuth t = AuthenticateReq
  (t, \tok req -> req { headers = ("Api-Key", tok) : headers req })

type APIRoute rest = "api" :> "1.4" :> AuthProtect "Api-Key" :> rest
type Wire = '[JSON]

newtype CommissionId = CommissionId Int
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

newtype ConversionId = ConversionId Int
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

newtype PayoutId = PayoutId Int
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

newtype ProgramId = ProgramId Text
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

data NewClick = NewClick
  { newClickId :: Text
  } deriving (Show)

mkJson ''NewClick

data OverrideCommission = OverrideCommission
  { overrideCommissionSubAmount :: Double
  , overrideCommissionComissionType :: Text
  } deriving (Show)

mkJson ''OverrideCommission

data NewConversion = NewConversion
  { newConversionClick :: Maybe NewClick
  , newConversionCoupon :: Text
  , newConversionVisitorId :: Maybe Text
  , newConversionExternalId :: Text
  , newConversionAmount :: Double
  , newConversionCommissionType :: Text
  , newConversionCommissions :: Maybe [OverrideCommission]
  , newConversionMetadata :: Object
  , newConversionProgramGroup :: ProgramId
  } deriving (Show)

mkJson ''NewConversion

data Click = Click
  { clickCreatedAt :: UTCTime
  } deriving (Show)

mkJson ''Click

data ConversionCommission = ConversionCommission
  { conversionCommissionCreatedAt :: UTCTime
  , conversionCommissionId :: CommissionId
  , conversionCommissionAmount :: Double
  , conversionCommissionApproved :: Maybe Bool
  , conversionCommissionType :: Text
  , conversionCommissionPayout :: Maybe PayoutId
  , conversionCommissionCommissionType :: Text
  , conversionCommissionConversionSubAmount :: Double
  , conversionCommissionComment :: Maybe Text
  } deriving (Show)

mkJson ''ConversionCommission

data Conversion = Conversion
  { conversionId :: ConversionId
  , conversionMetaData :: Object
  , conversionExternalId :: Text
  , conversionAmount :: Double
  , conversionClick :: Maybe Click
  , conversionCommissions :: [ConversionCommission]
  } deriving (Show)

mkJson ''Conversion

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
  , companyAddress :: Maybe Address
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

data Program = Program
  { programId :: ProgramId
  , programTitle :: Text
  , programCurrency :: Text
  , programCookieTime :: Int
  } deriving (Show)

mkJson ''Program

data Commission = Commission
  { commissionId :: CommissionId
  , commissionCreatedAt :: UTCTime
  , commissionAmount :: Double
  , commissionApproved :: Maybe Bool
  , commissionType :: Text
  , commissionCommissionType :: Text
  , commissionConversionSubAmount :: Double
  , commissionComment :: Text
  } deriving (Show)

mkJson ''Commission

type GetConversion = APIRoute ("conversions" :> Capture "conversion_id" ConversionId :> Get Wire Conversion)
type ListConversions = APIRoute
  ("conversions" :>
  QueryParam "program_id" Text :>
  QueryParam "external_id" Text :>
  QueryParam "affiliate_id" AffiliateId :>
  QueryParam "pending" Bool :>
  QueryParam "date_from" Day :>
  QueryParam "date_to" Day :>
  TrailingSlash :>
  Get Wire [Conversion])
type CreateConversion = APIRoute
  ("conversions" :>
  QueryParam "override_max_cookie_time" Bool :>
  ReqBody Wire NewConversion :>
  TrailingSlash :>
  Post Wire Conversion)
type AddCommissionToConversion = APIRoute
  ("conversions" :>
  Capture "conversion_id" ConversionId :>
  "commissions" :>
  ReqBody Wire Object :>
  TrailingSlash :>
  Post Wire [Object])

type API =
  GetConversion :<|>
  ListConversions :<|>
  CreateConversion :<|>
  AddCommissionToConversion :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            TrailingSlash :>
            Get Wire Object) :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            ReqBody Wire Object :>
            TrailingSlash :>
            PostNoContent '[] NoContent) :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            Capture "key" Text :>
            TrailingSlash :>
            Get Wire Object) :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            Capture "key" Text :>
            ReqBody Wire Object :>
            TrailingSlash :>
            PostNoContent '[] NoContent) :<|>
  APIRoute ("conversions" :>
            Capture "conversion_id" ConversionId :>
            "meta-data" :>
            Capture "key" Text :>
            TrailingSlash :>
            DeleteNoContent '[] NoContent) :<|>
  APIRoute ("commissions" :>
            Capture "commission_id" CommissionId :>
            TrailingSlash :>
            Get Wire Commission) :<|>
  APIRoute ("commissions" :>
            Capture "commission_id" CommissionId :>
            ReqBody Wire Commission :>
            TrailingSlash :>
            PutNoContent '[] NoContent) :<|>
  APIRoute ("commissions" :>
            Capture "commission_id" CommissionId :>
            "approval" :>
            TrailingSlash :>
            PutNoContent '[] NoContent) :<|>
  APIRoute ("commissions" :>
            Capture "commission_id" CommissionId :>
            "approval" :>
            TrailingSlash :>
            DeleteNoContent '[] NoContent) :<|>
  APIRoute ("affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            TrailingSlash :>
            Get Wire Affiliate) :<|>
  APIRoute ("affiliates" :>
            QueryParam "click_id" Text :>
            QueryParam "source_id" Text :>
            QueryParam "email" Text :>
            TrailingSlash :>
            Get Wire [Affiliate]) :<|>
  APIRoute ("affiliates" :>
            ReqBody Wire Affiliate :>
            TrailingSlash :>
            Post Wire Affiliate) :<|>
  APIRoute ("affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            "meta-data" :>
            TrailingSlash :>
            Get Wire Object) :<|>
  APIRoute ("affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            "meta-data" :>
            ReqBody Wire Object :>
            TrailingSlash :>
            PutNoContent '[] NoContent) :<|>
  -- Set meta data
  -- Delete meta data
  -- (MLM) Set parent
  -- (MLM) Remove parent
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            TrailingSlash :>
            Get Wire Program) :<|>
  APIRoute ("programs" :>
            QueryParam "asset_id" Text :>
            TrailingSlash :>
            Get Wire [Program]) :<|>
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            ReqBody Wire Object :>
            TrailingSlash :>
            Post Wire Object) :<|>
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            "affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            "approval" :>
            TrailingSlash :>
            PutNoContent '[] NoContent) :<|>
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            "affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            "approval" :>
            TrailingSlash :>
            DeleteNoContent '[] NoContent) :<|>
  APIRoute ("programs" :>
            Capture "program_id" ProgramId :>
            "affiliates" :>
            Capture "affiliate_id" AffiliateId :>
            TrailingSlash :>
            Get Wire Object) :<|>
  APIRoute ("payouts" :>
            Capture "payout_id" Text :>
            TrailingSlash :>
            Get Wire Object) :<|>
  APIRoute ("payouts" :>
            TrailingSlash :>
            Get Wire [Object]) :<|>
  APIRoute ("payouts" :>
            ReqBody Wire Object :>
            TrailingSlash :>
            Post Wire [Object]) :<|>
  APIRoute ("payouts" :>
            Capture "payout_id" Text :>
            "paid" :>
            TrailingSlash :>
            PutNoContent '[] NoContent) :<|>
  APIRoute ("payouts" :>
            Capture "payout_id" Text :>
            "paid" :>
            TrailingSlash :>
            DeleteNoContent '[] NoContent)

-- | Value-level representation of API.
api :: Proxy API
api = Proxy

getConversion'
  :: TapfiliateAuth
     -> ConversionId -> ClientM Conversion

listConversions'
  :: TapfiliateAuth
     -> Maybe Text
     -> Maybe Text
     -> Maybe AffiliateId
     -> Maybe Bool
     -> Maybe Day
     -> Maybe Day
     -> ClientM [Conversion]

createConversion'
  :: TapfiliateAuth
     -> Maybe Bool -> NewConversion -> ClientM Conversion

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
     -> CommissionId -> ClientM Commission

updateCommission'
  :: TapfiliateAuth
     -> CommissionId -> Commission -> ClientM NoContent

approveCommission'
  :: TapfiliateAuth
     -> CommissionId -> ClientM NoContent

disapproveCommission'
  :: TapfiliateAuth
     -> CommissionId -> ClientM NoContent

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
