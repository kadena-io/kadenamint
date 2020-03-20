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
import Control.Lens                                     (_Right, _Just, imap, makeLenses, (&), (^.), (.~), (%~))
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
import Text.URI.Lens                                    (authHost, authPort, uriAuthority)
import qualified Toml

import Prelude hiding (log)

import Kadenamint.Common
import Kadenamint.Tendermint.Config

data TendermintNode = TendermintNode
  { _tendermintNode_home        :: Text
  , _tendermintNode_config      :: Config
  , _tendermintNode_id          :: Text
  } deriving (Eq, Ord, Show, Generic)

newtype GlobalFlags = GlobalFlags
  { _globalFlags_home :: Text
  } deriving (Eq, Ord, Read, Show, Generic)

data NetworkFlags = NetworkFlags
  { _networkFlags_validators    :: Word
  , _networkFlags_output        :: Text
  , _networkFlags_populatePeers :: Bool
  } deriving (Eq, Ord, Read, Show, Generic)

concat <$> traverse makeLenses
  [ ''TendermintNode
  , ''GlobalFlags
  , ''NetworkFlags
  ]

mkNetworkFlags :: Text -> Word -> NetworkFlags
mkNetworkFlags networkRoot size = NetworkFlags
  { _networkFlags_validators    = size
  , _networkFlags_output        = networkRoot
  , _networkFlags_populatePeers = True
  }

loadNode :: MonadIO m => Text -> m TendermintNode
loadNode home = pure (TendermintNode home)
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

genesisFile :: TendermintNode -> Sh.FilePath
genesisFile n = configDir n </> ("genesis.json" :: Text)

configDir :: TendermintNode -> Sh.FilePath
configDir n = _tendermintNode_home n </> ("config" :: Text)

tendermintPath :: Sh.FilePath
tendermintPath = Sh.fromText $ T.pack $(staticWhich "tendermint")

tendermint :: GlobalFlags -> Text -> [Text] -> Sh Text
tendermint gf tmCmd cmdArgs = run tendermintPath $ tmArgs <> [tmCmd] <> cmdArgs
  where
    tmArgs = ["--home", _globalFlags_home gf]

tendermintNetwork :: NetworkFlags -> Sh Text
tendermintNetwork nf = run tendermintPath $ ("testnet" :) $
  [ "--v", tshow $ _networkFlags_validators nf
  , "--o", _networkFlags_output nf
  , "--starting-ip-address", localhost
  ] <> bool [] ["--populate-persistent-peers"] (_networkFlags_populatePeers nf)

tendermintNode :: GlobalFlags -> Sh Text
tendermintNode gf = tendermint gf "node" []

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
  { _nodePorts_p2p :: Word
  , _nodePorts_rpc :: Word
  , _nodePorts_abci :: Word
  } deriving (Eq, Ord, Read, Show, Generic)

nodePortsOffset :: Word
nodePortsOffset = 10

nthNodePorts :: Word -> NodePorts
nthNodePorts index =
  let offset = 26656 + nodePortsOffset * index
  in NodePorts offset (offset + 1) (offset + 2)

extraNodePorts :: NodePorts
extraNodePorts = nthNodePorts (negate 1)

addNode :: MonadIO m => Text -> Text -> NodePorts -> TendermintNode -> m TendermintNode
addNode home moniker ports preExistingNode = shelly $ do
  void $ tendermint (GlobalFlags home) "init" []
  n <- loadNode home

  cp (genesisFile preExistingNode) (configDir n)
  let
    newCfg = _tendermintNode_config preExistingNode
      & config_moniker .~ moniker
      & updatePorts ports
  storeConfig home newCfg

  pure $ n &
    tendermintNode_config .~ newCfg

runNodeDir :: MonadIO m => (TendermintNode -> IO ()) -> Text -> m ()
runNodeDir withNode dir = loadNode dir >>= runNode withNode

runNode :: MonadIO m => (TendermintNode -> IO ()) -> TendermintNode -> m ()
runNode withNode n = void $ do
  initProcess

  liftIO $ withAsync (withNode n) $ \_ ->
    flip runReaderT (coreEnv $ Just $ _config_moniker $ _tendermintNode_config n) $ do
      log "Launching" Nothing
      shelly $ tendermintNode $ GlobalFlags $ _tendermintNode_home n

initNetwork :: MonadIO m => Text -> Word -> m [TendermintNode]
initNetwork root size = shelly $ do
  void $ tendermintNetwork $ mkNetworkFlags root size
  for [0..size-1] $ \i -> do
    let
      home = root <> "/node" <> tshow i
    n <- loadNode home
    let
      imapWord f = imap $ \x y -> f (toEnum x) y
      oldCfg = n ^. tendermintNode_config
      ports = nthNodePorts i

      peers = T.splitOn "," $ oldCfg ^. config_p2p . configP2P_persistentPeers
      peers' = T.intercalate "," $ flip imapWord peers $ \j peer ->
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
          & config_consensus . configConsensus_createEmptyBlocksInterval .~ "10s"

    storeConfig home cfg
    pure $ n & tendermintNode_config .~ cfg

updatePorts :: NodePorts -> Config -> Config
updatePorts (NodePorts p2p rpc abci) cfg = cfg
  & config_p2p . configP2P_laddr %~ f p2p
  & config_rpc . configRPC_laddr %~ f rpc
  & config_proxyApp %~ f abci
  where
    f p h = h
      & uriAuthority . _Right . authPort . _Just .~ p
      & uriAuthority . _Right . authHost .~ localhostRText

withNetwork
  :: (TendermintNode -> IO ())
  -> Word
  -> (Text -> [TendermintNode] -> IO ())
  -> IO ()
withNetwork withNode size f = shelly $ withTmpDir $ \(toTextIgnore -> root) -> do
  genesisNodes <- initNetwork root size

  flip runReaderT (coreEnv Nothing) $
    log ("Network of size " <> tshow size <> " has been setup at " <> root) Nothing

  liftIO $ withAsync (f root genesisNodes) $ \_ ->
    forConcurrently_ genesisNodes (runNode withNode)
