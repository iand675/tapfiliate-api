{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Network.Tapfiliate
  ( API
  , api
  , mkClient
  , ConsulClient(..)
  , ConsulM
  , NodeName(..)
  , Peer(..)
  , KeyPath(..)
  , KeyPathPiece(..)
  , keyPath
  , runConsul
  -- * ACLs
  -- createACLToken
  -- updateACLToken
  -- destroyACLToken
  -- getACLToken
  -- cloneACLToken
  -- listACLTokens
  -- getACLReplicationStatus
  -- * Agent
  -- getLocalChecks
  -- getLocalServices
  -- getKnownMembers
  -- getSelf
  -- setNodeMaintenance
  -- joinNode
  -- forceNodeLeave
  -- registerCheck'
  -- deregisterCheck'
  -- setCheckPass
  -- setCheckWarn
  -- setCheckFail
  -- setCheckStatus
  -- registerService
  -- deregisterService
  -- setServiceMaintenance
  -- * Catalog
  -- registerCatalogItem
  -- deregisterCatalogItem
  , listDatacenters'
  , CatalogNode(..)
  , CatalogAddresses(..)
  , listNodes'
  , listServices'
  , ServiceNode(..)
  , listServiceNodes'
  -- listNodeServices
  -- * Events
  , Event(..)
  , Node(..)
  , Service(..)
  , fireEvent'
  , listEvents'
  -- * Health Checks
  , HealthCheck(..)
  , getNodeChecks
  , getNodeChecks'
  , getServiceChecks'
  , ServiceHealth(..)
  , getServiceHealth'
  , HealthCheckState(..)
  , getChecksByState'
  -- * Key / Value Store
  , ConsulValue(..)
  , getValue
  , getValue'
  -- updateValue'
  -- , deleteValue'
  -- performTransaction'
  -- * Network Coordinates
  , Coordinate(..)
  , CoordinateData(..)
  , DatacenterCoordinates(..)
  , listDatacenterCoordinates'
  , listNodeCoordinates'
  -- * Operator
  -- getRaftConfiguration
  -- deletePeer
  -- * Prepared Queries
  -- createPreparedQuery
  -- listPreparedQueries
  -- executePreparedQuery
  -- explainPreparedQuery
  -- * Sessions
  , NewSession(..)
  , SessionId(..)
  , NewSessionResponse(..)
  , createSession'
  , destroySession'
  -- querySession
  -- listNodeSessions
  -- listAllActiveSessions
  , renewSession'
  -- * Snapshots
  -- getStateSnapshot
  -- restoreStateSnapshot
  -- * Status
  , currentLeader
  , currentLeader'
  , listPeers
  , listPeers'
  ) where

import Protolude

import Data.Aeson
import Data.Coerce
import qualified Data.Text as T
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Network.Tapfiliate.Internal
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client


newtype NodeName = NodeName Text
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

newtype Peer = Peer Text
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

newtype KeyPath = KeyPath [KeyPathPiece]
  deriving (Show, Eq, Generic, Ord, Hashable, NFData, ToJSON, FromJSON)

newtype KeyPathPiece = KeyPathPiece Text
  deriving (Show, Eq, Generic, Ord, Hashable, NFData, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

keyPath :: [Text] -> KeyPath
keyPath = coerce

instance ToHttpApiData KeyPath where
  toQueryParam (KeyPath ts) = T.intercalate "/" $ coerce ts

data CatalogAddresses = CatalogAddresses
  { catalogAddressesLan :: Text
  , catalogAddressesWan :: Text
  } deriving (Show)

mkJson ''CatalogAddresses

data CatalogNode = CatalogNode
  { catalogNodeNode :: NodeName
  , catalogNodeAddress :: Text
  , catalogNodeTaggedAddresses :: CatalogAddresses
  } deriving (Show)

mkJson ''CatalogNode

data ServiceNode = ServiceNode
  { serviceNodeAddress :: Text
  , serviceNodeTaggedAddresses :: Maybe CatalogAddresses
  , serviceNodeCreateIndex :: Int
  , serviceNodeModifyIndex :: Int
  , serviceNodeNode :: NodeName
  , serviceNodeServiceAddress :: Text
  , serviceNodeServiceEnableTagOverride :: Bool
  , serviceNodeServiceID :: Text
  , serviceNodeServiceName :: Text
  , serviceNodeServicePort :: Int
  , serviceNodeServiceTags :: [Text]
  } deriving (Show)

mkJson ''ServiceNode

data Event = Event
  { eventID :: Text
  , eventName :: Text
  , eventPayload :: Maybe Text
  , eventNodeFilter :: Text
  , eventServiceFilter :: Text
  , eventTagFilter :: Text
  , eventVersion :: Int
  , eventLTime :: Int
  } deriving (Show)

mkJson ''Event

data HealthCheck = HealthCheck
  { healthCheckNode :: NodeName
  , healthCheckCheckID :: Text
  , healthCheckName :: Text
  , healthCheckStatus :: Text
  , healthCheckNotes :: Text
  , healthCheckOutput :: Text
  , healthCheckServiceID :: Text
  , healthCheckServiceName :: Text
  } deriving (Show)

mkJson ''HealthCheck

data Node = Node
  { nodeNode :: NodeName
  , nodeAddress :: Text
  , nodeTaggedAddresses :: HashMap Text Text
  } deriving (Show)

mkJson ''Node

data Service = Service
  { serviceID :: Text
  , serviceService :: Text
  , serviceTags :: Maybe [Text]
  , serviceAddress :: Text
  , servicePort :: Int
  } deriving (Show)

mkJson ''Service

data ServiceHealth = ServiceHealth
  { serviceHealthNode :: Node
  , serviceHealthService :: Service
  , serviceHealthChecks :: [HealthCheck]
  } deriving (Show)

mkJson ''ServiceHealth

data HealthCheckState
  = AnyState
  | HealthCheckIsPassing
  | HealthCheckIsWarning
  | HealthCheckIsCritical
  deriving (Show)

instance ToHttpApiData HealthCheckState where
  toUrlPiece s = case s of
    AnyState -> "any"
    HealthCheckIsPassing -> "passing"
    HealthCheckIsWarning -> "warning"
    HealthCheckIsCritical -> "critical"

data ConsulValue = ConsulValue
  { consulValueCreateIndex :: Int
  , consulValueModifyIndex :: Int
  , consulValueLockIndex :: Int
  , consulValueKey :: Text
  , consulValueFlags :: Int
  , consulValueValue :: Text
  , consulValueSession :: Maybe Text
  } deriving (Show)

mkJson ''ConsulValue

data CoordinateData = CoordinateData
  { coordinateDataAdjustment :: Double
  , coordinateDataError :: Double
  , coordinateDataHeight :: Double
  , coordinateDataVec :: [Double]
  } deriving (Show)

mkJson ''CoordinateData

data Coordinate = Coordinate
  { coordinateNode :: NodeName
  , coordinateCoord :: CoordinateData
  } deriving (Show)

mkJson ''Coordinate

data DatacenterCoordinates = DatacenterCoordinates
  { datacenterCoordinatesDatacenter :: Text
  , datacenterCoordinatesCoordinates :: [Coordinate]
  } deriving (Show)

mkJson ''DatacenterCoordinates

data NewSession = NewSession
  { newSessionLockDelay :: Maybe Text
  , newSessionName :: Maybe Text
  , newSessionNode :: Maybe Text
  , newSessionChecks :: [Text]
  , newSessionBehavior :: Maybe Text
  , newSessionTTL :: Maybe Text
  } deriving (Show)

mkJson ''NewSession

newtype SessionId = SessionId Text
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

data NewSessionResponse = NewSessionResponse
  { newSessionResponseID :: SessionId
  } deriving (Show)

mkJson ''NewSessionResponse

type ConsulM = ClientM
type Wire = '[JSON]
type ApiV1 rest = "v1" :> rest
type Catalog rest = ApiV1 ("catalog" :> rest)
type EventEndpoint rest = ApiV1 ("event" :> rest)
type Health rest = ApiV1 ("health" :> rest)

newtype Index = Index Int64
  deriving (Show, Eq, Generic, Ord, Hashable, ToHttpApiData, FromHttpApiData, NFData, ToJSON, FromJSON)

type ConsulHeader a = Headers '[Header "X-Consul-Index" Index] a

type API =
  Catalog ("datacenters" :> Get Wire [Text]) :<|>
  Catalog ("nodes" :> Get Wire [CatalogNode]) :<|>
  Catalog ("services" :> Get Wire (HashMap Text [Text])) :<|>
  Catalog ("service" :> Capture "service" Text :> QueryParam "tag" Text :> QueryParam "near" Text :> Get Wire [ServiceNode]) :<|>
  EventEndpoint ("fire" :> Capture "name" Text :> ReqBody '[OctetStream] LByteString :> QueryParam "node" NodeName :> QueryParam "service" Text :> QueryParam "tag" Text :> Put Wire Event) :<|>
  EventEndpoint ("list" :> QueryParam "name" Text :> Get Wire [Event]) :<|>
  Health ("node" :> Capture "node" NodeName :> Get Wire [HealthCheck]) :<|>
  Health ("checks" :> Capture "service" Text :> Get Wire [HealthCheck]) :<|>
  Health ("service" :> Capture "service" Text :> Get Wire [ServiceHealth]) :<|>
  Health ("state" :> Capture "state" HealthCheckState :> Get Wire [HealthCheck]) :<|>
  ApiV1 ("kv" :> CaptureAll "key" KeyPathPiece :> QueryParam "dc" Text :> QueryParam "token" Text :> QueryFlag "recurse" :> Get Wire (ConsulHeader [ConsulValue])) :<|>
  ApiV1 ("coordinate" :> "datacenters" :> Get Wire [DatacenterCoordinates]) :<|>
  ApiV1 ("coordinate" :> "nodes" :> Get Wire [Coordinate]) :<|>
  ApiV1 ("session" :> "create" :> ReqBody Wire NewSession :> Put Wire SessionId) :<|>
  ApiV1 ("session" :> "destroy" :> Capture "session" SessionId :> Put '[] NoContent) :<|>
  ApiV1 ("session" :> "renew" :> Capture "session" SessionId :> Put Wire Object) :<|>
  -- TODO Text -> network address
  ApiV1 ("status" :> "leader" :> Get Wire Peer) :<|>
  -- TODO Text -> network address
  ApiV1 ("status" :> "peers" :> Get Wire [Peer])

-- | Value-level representation of API.
api :: Proxy API
api = Proxy

listDatacenters'
  :: ClientM [Text]

listNodes'
  :: ClientM [CatalogNode]

listServices'
  :: ClientM (HashMap Text [Text])

listServiceNodes'
  :: Text
  -> Maybe Text
  -> Maybe Text
  -> ClientM [ServiceNode]

fireEvent'
  :: Text
  -> LByteString
  -> Maybe NodeName
  -> Maybe Text
  -> Maybe Text
  -> ClientM Event

listEvents'
  :: Maybe Text
  -> ClientM [Event]

getNodeChecks'
  :: NodeName
  -> ClientM [HealthCheck]

getServiceChecks'
  :: Text
  -> ClientM [HealthCheck]

getServiceHealth'
  :: Text
  -> ClientM [ServiceHealth]

getChecksByState'
  :: HealthCheckState
  -> ClientM [HealthCheck]

getValue'
  :: [KeyPathPiece]
  -> Maybe Text
  -> Maybe Text
  -> Bool
  -> ClientM (ConsulHeader [ConsulValue])

createSession'
  :: NewSession
  -> ClientM SessionId

destroySession'
  :: SessionId
  -> ClientM NoContent

renewSession'
  :: SessionId
  -> ClientM Object

currentLeader'
  :: ClientM Peer

listPeers'
  :: ClientM [Peer]

listDatacenterCoordinates'
  :: ClientM [DatacenterCoordinates]

listNodeCoordinates'
  :: ClientM [Coordinate]

(
 listDatacenters' :<|>
 listNodes' :<|>
 listServices' :<|>
 listServiceNodes' :<|>
 fireEvent' :<|>
 listEvents' :<|>
 getNodeChecks' :<|>
 getServiceChecks' :<|>
 getServiceHealth' :<|>
 getChecksByState' :<|>
 getValue' :<|>
 listDatacenterCoordinates' :<|>
 listNodeCoordinates' :<|>
 createSession' :<|>
 destroySession' :<|>
 renewSession' :<|>
 -- sessionInfo' :<|>
 currentLeader' :<|>
 listPeers') = client api

newtype ConsulClient = ConsulClient { fromConsulClient :: ClientEnv }

runConsul :: ConsulClient -> ConsulM a -> ConsulResponse a
runConsul c m = runClientM m (coerce c)

type ConsulResponse r = IO (Either ServantError r)

mkClient :: IO ConsulClient
mkClient = do
  man <- getGlobalManager
  u <- parseBaseUrl "http://localhost:8500"
  return $ ConsulClient $ ClientEnv man u

getNodeChecks :: ConsulClient -> NodeName -> ConsulResponse [HealthCheck]
getNodeChecks c n = runClientM (getNodeChecks' n) $ coerce c

getValue :: ConsulClient -> [KeyPathPiece] -> Maybe Text -> Maybe Text -> Bool -> ConsulResponse (ConsulHeader [ConsulValue])
getValue c p dc tok recurse = runClientM (getValue' p dc tok recurse) $ coerce c

currentLeader :: ConsulClient -> ConsulResponse Peer
currentLeader = runClientM currentLeader' . coerce

listPeers :: ConsulClient -> ConsulResponse [Peer]
listPeers = runClientM listPeers' . coerce
