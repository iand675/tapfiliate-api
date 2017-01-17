module Main
  ( main
  ) where

import Protolude

import Control.Exception
import qualified Data.HashMap.Strict as H
import Network.Tapfiliate
import Test.Tasty (defaultMainWithIngredients, defaultIngredients, TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.Runners.AntXML

main :: IO ()
main = defaultMainWithIngredients (antXMLRunner : defaultIngredients) tests

testAPI :: TapfiliateM a -> IO a
testAPI m = do
  c <- mkClient
  r <- runTapfiliate c m
  case r of
    Left e -> throw e
    Right x -> return x

tests :: TestTree
tests = testGroup "Tapfiliate API"
  [ testCase "List programs" $ do
      auth <- getEnvAuth
      void $ testAPI $ do
        listPrograms' auth Nothing
  , testCase "List affiliates" $ do
      auth <- getEnvAuth
      void $ testAPI $ do
        listAffiliates' auth Nothing Nothing Nothing
  , testCase "List conversions" $ do
      auth <- getEnvAuth
      void $ testAPI $ do
        listConversions' auth Nothing Nothing Nothing Nothing Nothing Nothing
  , testCase "List payouts" $ do
      auth <- getEnvAuth
      void $ testAPI $ do
        listPayouts' auth
  -- Note: this depends on a program being titled "Test Program"
  , testCase "Create affiliate for program" $ do
      auth <- getEnvAuth
      void $ testAPI $ do
        r <- listPrograms' auth Nothing
        let program = head $ filter (\p -> programTitle p == "Test Program") r
        createAffiliate' auth $ Affiliate
          { affiliateId = AffiliateId ""
          , affiliateFirstname = "first"
          , affiliateLastname = "last"
          , affiliateEmail = "1234@example.com"
          , affiliatePassword = Nothing
          , affiliateCompany = Nothing
          , affiliateMetaData = H.empty
          }
        addAffiliateToProgram'
  ]

