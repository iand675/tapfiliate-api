module Main
  ( main
  ) where

import Protolude

import Control.Exception
import Network.Consul
import System.Process
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit

main :: IO ()
main = bracket
  (spawnProcess "consul" ["agent", "-dev"])
  (\p -> interruptProcessGroupOf p >> waitForProcess p)
  (\_ -> threadDelay 2500000 >> defaultMain tests)

testConsul :: ConsulM a -> IO a
testConsul m = do
  c <- mkClient
  r <- runConsul c m
  case r of
    Left e -> throw e
    Right x -> return x

tests :: TestTree
tests = testGroup "Consul HTTP API"
  [ testCase "GET /v1/catalog/datacenters" $ do
      r <- testConsul listDatacenters'
      print r

  , testCase "GET /v1/catalog/services" $ do
      r <- testConsul listServices'
      print r

  , testCase "GET /v1/catalog/service/<service>" $ do
      r <- testConsul $ listServiceNodes'
           "consul"
           Nothing
           Nothing
      print r

  , testCase "GET /v1/event/fire/<name>" $ do
      r <- testConsul $ fireEvent'
           "test-event"
           "test body"
           Nothing
           Nothing
           Nothing
      print r

  , testCase "GET /v1/event/list" $ do
      r <- testConsul $ listEvents' Nothing
      print r

  , testCase "GET /v1/coordinate/datacenters" $ do
      r <- testConsul listDatacenterCoordinates'
      print r

  , testCase "GET /v1/coordinate/nodes" $ do
      testConsul listNodeCoordinates' >>= print

  , testCase "GET /v1/status/leader" $ do
      l <- testConsul currentLeader'
      print l

  , testCase "GET /v1/status/peers" $ do
      peers <- testConsul listPeers'
      print peers
  ]
