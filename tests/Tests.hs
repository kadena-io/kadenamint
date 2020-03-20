{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Control.Lens ((^.))
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant ((:<|>)(..))
import Servant.Client (BaseUrl(..),  ClientError, Scheme(Http), client, mkClientEnv, runClientM)
import Test.Hspec (describe, hspec, it,  parallel, shouldBe)

import Kadenamint
import Kadenamint.Common
import Kadenamint.Pact
import Kadenamint.Tendermint

test :: IO ()
test = do
  initProcess
  m <- newManager defaultManagerSettings
  let
    getInfo :<|> _ = client kadenamintApi

    apiCall endpoint kn = runClientM endpoint env
      where
        apiPort = fromEnum $ _kadenamintNode_pactAPIPort kn
        nodeUrl = BaseUrl Http "localhost" apiPort ""
        env = mkClientEnv m nodeUrl

  withKadenamintNetwork 3 $ \root -> \case
    nodes -> do
      let separator = T.replicate 80 "="
      T.putStrLn $ T.unlines
        [ separator
        , "Running kadenamint network at " <> root
        , separator
        ]
      sleep 8
      testInfo nodes $ apiCall getInfo

testInfo :: [KadenamintNode] -> (KadenamintNode -> IO (Either ClientError NodeInfo)) -> IO ()
testInfo nodes getInfo = hspec $ do
  describe "info endpoint" $ parallel $ for_ nodes $ \n -> do
    let moniker = n ^. kadenamintNode_tendermint . tendermintNode_config . config_moniker
    it ("has expected response for #" <> T.unpack moniker) $ do
      x <- getInfo n
      x `shouldBe` Right (NodeInfo Version_Devnet_00 "0.0" ["0"] 1)
