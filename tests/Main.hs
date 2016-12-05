module Main
  ( main
  ) where

import Protolude

import Control.Exception
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
  [ testCase "List affiliates" $ do
      auth <- getEnvAuth
      void $ testAPI $ do
        listAffiliates' auth Nothing Nothing Nothing
  ]
