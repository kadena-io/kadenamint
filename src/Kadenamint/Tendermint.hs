{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Kadenamint.Tendermint
  ( module Kadenamint.Tendermint
  , module Kadenamint.Tendermint.Config
  ) where

import Control.Concurrent.Async                         (forConcurrently_, withAsync)
import Control.Lens                                     (imap, makeLenses, (&), (^.), (.~))
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (ReaderT(..), runReaderT)
import Data.Bool                                        (bool)
import Data.Functor                                     (void)
import Data.Text                                        (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable                                 (for)
import GHC.Generics
import Shelly                                           (Sh, cp, shelly, silently, run, toTextIgnore, withTmpDir, (</>))
import qualified Shelly as Sh
import System.Console.ANSI                              (SGR(..), ConsoleLayer(..))
import System.Which                                     (staticWhich)
import Text.Read as T                                   (readMaybe)
import qualified Toml

import Prelude hiding (log)

import Kadenamint.Common
import Kadenamint.Tendermint.Config

data InitializedNode = InitializedNode
  { _initializedNode_home        :: Text
  , _initializedNode_config      :: Config
  , _initializedNode_id          :: Text
  } deriving (Eq, Ord, Read, Show, Generic)

type Address = Text
type Peer = (Text, Address)

newtype GlobalFlags = GlobalFlags
  { _globalFlags_home :: Text
  } deriving (Eq, Ord, Read, Show, Generic)

data NetworkFlags = NetworkFlags
  { _networkFlags_validators    :: Int
  , _networkFlags_output        :: Text
  , _networkFlags_populatePeers :: Bool
  } deriving (Eq, Ord, Read, Show, Generic)

concat <$> traverse makeLenses
  [ ''InitializedNode
  , ''GlobalFlags
  , ''NetworkFlags
  ]

mkNetworkFlags :: Text -> Int -> NetworkFlags
mkNetworkFlags networkRoot size = NetworkFlags
  { _networkFlags_validators    = size
  , _networkFlags_output        = networkRoot
  , _networkFlags_populatePeers = True
  }

loadNode :: MonadIO m => Text -> m InitializedNode
loadNode home = pure (InitializedNode home)
  <*> loadConfig home
  <*> shelly (silently $ fmap T.strip $ tendermint (GlobalFlags home) "show_node_id" [])

loadConfig :: MonadIO m => Text -> m Config
loadConfig home = liftIO $ do
  toml <- T.readFile $ T.unpack $ home <> "/config" <> "/config.toml"
  case Toml.decode configCodec toml of
    Left err -> do
      print toml
      print err
      error "Failed decoding"
    Right config -> pure config

storeConfig :: MonadIO m => Text -> Config -> m ()
storeConfig home = liftIO . T.writeFile (T.unpack $ home <> "/config" <> "/config.toml") . Toml.encode configCodec

genesisFile :: InitializedNode -> Sh.FilePath
genesisFile n = configDir n </> ("genesis.json" :: Text)

configDir :: InitializedNode -> Sh.FilePath
configDir n = _initializedNode_home n </> ("config" :: Text)

tendermint :: GlobalFlags -> Text -> [Text] -> Sh Text
tendermint gf tmCmd cmdArgs = runCmd $ tendermintCmd gf tmCmd cmdArgs

tendermintNetwork :: NetworkFlags -> Sh Text
tendermintNetwork = runCmd . tendermintNetworkCmd

tendermintNode :: GlobalFlags -> Sh Text
tendermintNode = runCmd . tendermintNodeCmd

runCmd :: (Sh.FilePath, [Text]) -> Sh Text
runCmd = uncurry run

plainCmd :: (Sh.FilePath, [Text]) -> Text
plainCmd (cmd, args) = T.intercalate " " $ Sh.toTextIgnore cmd : fmap singleQuotes args

tendermintPath :: Sh.FilePath
tendermintPath = Sh.fromText $ T.pack $(staticWhich "tendermint")

tendermintCmd :: GlobalFlags -> Text -> [Text] -> (Sh.FilePath, [Text])
tendermintCmd gf tmCmd cmdArgs = (tendermintPath,) $ tmArgs <> [tmCmd] <> cmdArgs
  where
    tmArgs = ["--home", _globalFlags_home gf]

tendermintNetworkCmd :: NetworkFlags -> (Sh.FilePath, [Text])
tendermintNetworkCmd nf = (tendermintPath,) $ ("testnet" :) $
  [ "--v", tshow $ _networkFlags_validators nf
  , "--o", _networkFlags_output nf
  , "--starting-ip-address", localhost
  ] <> bool [] ["--populate-persistent-peers"] (_networkFlags_populatePeers nf)

tendermintNodeCmd :: GlobalFlags -> (Sh.FilePath, [Text])
tendermintNodeCmd gf = tendermintCmd gf "node" []

coreEnv :: Maybe Text -> Env
coreEnv moniker = Env
  { _env_printer = \x ->
      sgrify [SetRGBColor Foreground red] $ mconcat
      [ "\n[CORE] "
      , maybe "" (\m -> "Node: " <> tshow m <> " | ") moniker
      , x
      ]
  }

data NodePorts = NodePorts
  { _nodePorts_p2p :: Int
  , _nodePorts_rpc :: Int
  , _nodePorts_abci :: Int
  } deriving (Eq, Ord, Read, Show, Generic)

nodePortsOffset :: Int
nodePortsOffset = 10

nthNodePorts :: Int -> NodePorts
nthNodePorts index =
  let offset = 26656 + nodePortsOffset * index
  in NodePorts offset (offset + 1) (offset + 2)

extraNodePorts :: NodePorts
extraNodePorts = nthNodePorts (negate 1)

addNode :: MonadIO m => Text -> Text -> NodePorts -> InitializedNode -> m InitializedNode
addNode home moniker ports preExistingNode = shelly $ do
  void $ tendermint (GlobalFlags home) "init" []
  n <- loadNode home

  cp (genesisFile preExistingNode) (configDir n)
  let
    newCfg = _initializedNode_config preExistingNode
      & config_moniker .~ moniker
      & updatePorts ports
  storeConfig home newCfg

  pure $ n &
    initializedNode_config .~ newCfg

runNodeDir :: MonadIO m => (InitializedNode -> IO ()) -> Text -> m ()
runNodeDir runABCI dir = loadNode dir >>= runNode runABCI

runNode :: MonadIO m => (InitializedNode -> IO ()) -> InitializedNode -> m ()
runNode runABCI n = void $ do
  liftIO $ withAsync (runABCI n) $ \_ ->
    flip runReaderT (coreEnv $ Just $ _config_moniker $ _initializedNode_config n) $ do
      log "Launching" Nothing
      shelly $ tendermintNode $ GlobalFlags $ _initializedNode_home n

initNetwork :: MonadIO m => Text -> Int -> m [InitializedNode]
initNetwork root size = shelly $ do
  void $ tendermintNetwork $ mkNetworkFlags root size
  for [0..size-1] $ \i -> do
    let
      home = root <> "/node" <> tshow i
    n <- loadNode home
    let
      oldCfg = n ^. initializedNode_config
      ports = nthNodePorts i

      peers = T.splitOn "," $ oldCfg ^. config_p2p . configP2P_persistentPeers
      peers' = T.intercalate "," $ flip imap peers $ \j peer ->
        let (nid, rest) = cleave "@" peer
            (_host, port) = cleave ":" rest
            port' = case T.readMaybe (T.unpack port) of
              Nothing -> error "parsing error"
              Just p -> p + j * nodePortsOffset
        in nid <> "@" <> localhost <> ":" <> tshow port'

    let cfg = oldCfg
          & config_moniker .~ "node" <> tshow i
          & updatePorts ports
          & config_p2p . configP2P_persistentPeers .~ peers'
          & config_p2p . configP2P_privatePeerIds .~ peers'
          & config_p2p . configP2P_addrBookStrict .~ False
          & config_p2p . configP2P_allowDuplicateIp .~ True
          & config_consensus . configConsensus_createEmptyBlocksInterval .~ "5s"

    storeConfig home cfg
    pure $ n & initializedNode_config .~ cfg

updatePorts :: NodePorts -> Config -> Config
updatePorts (NodePorts p2p rpc abci) cfg = cfg
  & config_p2p . configP2P_laddr .~ l p2p
  & config_rpc . configRPC_laddr .~ l rpc
  & config_proxyApp .~ l abci
  where
    l p = "tcp://" <> localhost <> ":" <> tshow p

withNetwork
  :: (InitializedNode -> IO ())
  -> Int
  -> (Text -> [InitializedNode] -> IO ())
  -> IO ()
withNetwork runABCI size f = shelly $ withTmpDir $ \(toTextIgnore -> root) -> do
  genesisNodes <- initNetwork root size

  flip runReaderT (coreEnv Nothing) $
    log ("Network of size " <> tshow size <> " has been setup at " <> root) Nothing

  liftIO $ withAsync (f root genesisNodes) $ \_ ->
    forConcurrently_ genesisNodes (runNode runABCI)
