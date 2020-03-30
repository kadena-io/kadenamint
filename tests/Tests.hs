{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Control.Error.Util (hush)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (BaseUrl(..),  ClientError, ClientM, Scheme(Http), mkClientEnv, runClientM)
import Test.Hspec (describe, hspec, it, parallel, shouldBe)
import Test.Hspec.Core.Spec (SpecM)

import Pact.Types.API (ListenerRequest(..), ListenResponse(..), Poll(..), PollResponses(..), RequestKeys(..), SubmitBatch(..))
import Pact.Types.Command (Command(..), CommandResult(..), PactResult(..), UserSig(..), cmdToRequestKey)
import Pact.Types.Exp (Literal(LDecimal, LString))
import Pact.Types.Hash (Hash(..), hash)
import Pact.Types.PactValue (PactValue(PLiteral))
import Pact.Types.Util (parseB64UrlUnpaddedText')

import Kadenamint
import Kadenamint.Common
import Kadenamint.Pact
import Kadenamint.Tendermint

shouldYield :: (Show e, Show b, Eq e, Eq b) => IO (Either e b) -> b -> IO ()
end `shouldYield` res = end >>= (`shouldBe` Right res)

test :: IO ()
test = do
  initProcess
  m <- newManager defaultManagerSettings
  let
    apiCall :: MonadIO m => KadenamintNode -> ClientM a -> m (Either ClientError a)
    apiCall kn endpoint = liftIO $ runClientM endpoint env
      where
        apiPort = fromEnum $ _kadenamintNode_pactAPIPort kn
        nodeUrl = BaseUrl Http "localhost" apiPort ""
        env = mkClientEnv m nodeUrl

  withThrowawayKadenamintNetwork 3 $ \root -> \case
    nodes@[n0, n1, n2] -> do
      let separator = T.replicate 80 "="
      T.putStrLn $ T.unlines
        [ separator
        , "Running kadenamint network at " <> root
        , separator
        ]
      sleep 8
      testInfo nodes apiCall
      sleep 4
      testCreate (n0, n1, n2) apiCall
    _ -> impossible

testInfo
  :: [KadenamintNode]
  -> ((forall a. KadenamintNode -> ClientM a -> IO (Either ClientError a)))
  -> IO ()
testInfo nodes apiCall = hspec $ do
  describe "info endpoint" $ parallel $ for_ nodes $ \n -> do
    let moniker = n ^. kadenamintNode_tendermint . tendermintNode_config . config_moniker
    it ("has expected response for #" <> T.unpack moniker) $ do
      apiCall n infoEndpoint `shouldYield` NodeInfo Version_Devnet_00 "0.0" ["0"] 1

mkCmd :: [UserSig] -> Text -> Command Text
mkCmd sigs payload = Command
  { _cmdPayload = payload
  , _cmdSigs = sigs
  , _cmdHash = hash $ T.encodeUtf8 payload
  }

testCommand
  :: (Command Text, CommandResult Hash)
  -> (Command Text, CommandResult Hash)
  -> (KadenamintNode, KadenamintNode, KadenamintNode)
  -> (forall a. KadenamintNode -> ClientM a -> IO (Either ClientError a))
  -> SpecM () ()
testCommand (cmd, cmdResult) (localCmd, localCmdResult) (a,b,c) apiCall = do
  let key = cmdToRequestKey cmd

  it "/poll fails before request" $ do
    apiCall a (pollEndpoint $ Poll $ pure key) `shouldYield` PollResponses mempty
  it "/send works" $ do
    apiCall a (sendEndpoint $ SubmitBatch $ pure cmd) `shouldYield` RequestKeys (pure key)
  it "/listen works" $ do
    apiCall b (listenEndpoint $ ListenerRequest key) `shouldYield` ListenResponse cmdResult
  it "/poll succeeds after request" $ do
    apiCall b (pollEndpoint $ Poll $ pure key) `shouldYield` PollResponses (HM.singleton key cmdResult)
  it "/local can read new state" $ do
    apiCall c (localEndpoint localCmd) `shouldYield` localCmdResult

testCreate
  :: (KadenamintNode, KadenamintNode, KadenamintNode)
  -> (forall a. KadenamintNode -> ClientM a -> IO (Either ClientError a))
  -> IO ()
testCreate (a,b,c) apiCall = hspec $ do
  describe "create account" $ do
    testCommand (createAccount, createResult) (checkBalances, balanceResult) (a,b,c) apiCall

    where
      createKey = cmdToRequestKey createAccount
      rawHash = fmap Hash . hush . parseB64UrlUnpaddedText'

      createResult = CommandResult
        { _crReqKey = createKey
        , _crTxId = Just 3
        , _crResult = PactResult (Right (PLiteral (LString "Write succeeded")))
        , _crGas = 0
        , _crLogs = rawHash "v4p_WR_XdUiJKZ7RR1117AL9N4BQUxhKeOWfo3mwSjQ"
        , _crContinuation = Nothing
        , _crMetaData = Nothing
        }

      balanceResult = CommandResult
        { _crGas = 0
        , _crResult = PactResult $ Right $ PLiteral $ LDecimal 123
        , _crReqKey = cmdToRequestKey checkBalances
        , _crLogs = rawHash "wsATyGqckuIvlm89hhd2j4t6RMkCrcwJe_oeCYr7Th8"
        , _crContinuation = Nothing
        , _crMetaData = Nothing
        , _crTxId = Nothing
        }

      createAccount = mkCmd
        [UserSig {_usSig = "43ef3939dc71a7a02e382cf96eb9ce58eed8a90b90a0a7b4253f3bb3916353293899d301c677936880d1e7c4d4f95efea78e23fef103da816e9be427a441d20f"}]
        "{\"networkId\":\"devnet00\",\"payload\":{\"exec\":{\"data\":{\"tempkeyset\":{\"pred\":\"keys-all\",\"keys\":[\"b9e712db89d070ad3f9d09ae41e419a01ddba355bcd7f0176fc5526c654fdfc3\"]}},\"code\":\"(coin.transfer-create \\\"sender00\\\" \\\"gththtrhththshtgxf\\\" (read-keyset \\\"tempkeyset\\\") 123.0 )\"}},\"signers\":[{\"addr\":\"368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca\",\"scheme\":\"ED25519\",\"pubKey\":\"368820f80c324bbc7c2b0610688a7da43e39f91d118732671cd9c7500ff43cca\",\"clist\":[{\"args\":[],\"name\":\"coin.GAS\"},{\"args\":[\"sender00\",\"gththtrhththshtgxf\",123],\"name\":\"coin.TRANSFER\"}]}],\"meta\":{\"creationTime\":1585069838,\"ttl\":28800,\"gasLimit\":600,\"chainId\":\"0\",\"gasPrice\":1.0e-5,\"sender\":\"sender00\"},\"nonce\":\"2020-03-24 17:10:52.505534063 UTC\"}"

      checkBalances = mkCmd
        [UserSig {_usSig = "803b04a44c0f28b74a14d5b7ee560bcdd79ba45b011d00ef90a9878205bbdf82339a7a191d02e9d4677cfd37682fa48a5bbc0eb63d40eea598af88c82a049b07"}]
        "{\"networkId\":\"devnet00\",\"payload\":{\"exec\":{\"data\":{},\"code\":\"(coin.get-balance \\\"gththtrhththshtgxf\\\")\"}},\"signers\":[{\"addr\":\"60cd9d8b10763372e9c535babd62d0a3b48dc6825d00690bd3cb27f8685cddab\",\"scheme\":\"ED25519\",\"pubKey\":\"60cd9d8b10763372e9c535babd62d0a3b48dc6825d00690bd3cb27f8685cddab\",\"clist\":[{\"args\":[],\"name\":\"coin.GAS\"}]}],\"meta\":{\"creationTime\":1585072881,\"ttl\":28800,\"gasLimit\":600,\"chainId\":\"0\",\"gasPrice\":1.0e-5,\"sender\":\"gththtrhththshtgxf\"},\"nonce\":\"2020-03-24 18:01:35.637855496 UTC\"}"
