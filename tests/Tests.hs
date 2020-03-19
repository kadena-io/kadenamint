{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Lens ((??))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>)(..))
import Servant.Client (BaseUrl(..),  ClientError, Scheme(Http), client, mkClientEnv, runClientM)
import Test.Hspec (describe, hspec, it, shouldBe)

import Kadenamint
import Kadenamint.Common
import Kadenamint.Pact

test :: IO ()
test = do
  initProcess
  m <- newManager defaultManagerSettings
  let nodeUrl = BaseUrl Http "localhost" 26659 ""
      env = mkClientEnv m nodeUrl
      endpoint = runClientM ?? env
      getInfo :<|> _ = client kadenamintApi

  withKadenamintNetwork 1 $ \root -> \case
    [_n0] -> do
      let separator = T.replicate 80 "="
      T.putStrLn $ T.unlines
        [ separator
        , "Running kadenamint network at " <> root
        , separator
        ]
      sleep 4
      testInfo $ endpoint getInfo
    _ -> impossible

testInfo :: IO (Either ClientError NodeInfo) -> IO ()
testInfo nodeEndpoint = hspec $ do
  describe "info endpoint" $ do
    it "has expected response" $ do
      x <- nodeEndpoint
      x `shouldBe` Right (NodeInfo Version_Devnet_00 "0.0" ["0"] 1)
