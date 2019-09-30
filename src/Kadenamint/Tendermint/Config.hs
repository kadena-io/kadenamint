{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kadenamint.Tendermint.Config where

import Control.Lens (makeLenses)
import Data.Text    (Text)
import GHC.Generics

import Toml (TomlCodec, (.=))
import qualified Toml

data Config = Config
  { _config_moniker :: Text
  , _config_proxyApp :: Text
  , _config_fastSync :: Bool
  , _config_genesisFile :: Text
  , _config_rpc :: ConfigRPC
  , _config_p2p :: ConfigP2P
  , _config_consensus :: ConfigConsensus
  } deriving (Eq, Ord, Read, Show, Generic)

data ConfigRPC = ConfigRPC
  { _configRPC_laddr :: Text
  } deriving (Eq, Ord, Read, Show, Generic)

data ConfigP2P = ConfigP2P
  { _configP2P_laddr :: Text
  , _configP2P_persistentPeers :: Text
  , _configP2P_privatePeerIds :: Text
  , _configP2P_addrBookStrict :: Bool
  , _configP2P_allowDuplicateIp :: Bool
  } deriving (Eq, Ord, Read, Show, Generic)

data ConfigConsensus = ConfigConsensus
  { _configConsensus_createEmptyBlocksInterval :: Text
  } deriving (Eq, Ord, Read, Show, Generic)

configCodec :: TomlCodec Config
configCodec = pure Config
  <*> Toml.text                 "moniker"                      .= _config_moniker
  <*> Toml.text                 "proxy_app"                    .= _config_proxyApp
  <*> Toml.bool                 "fast_sync"                    .= _config_fastSync
  <*> Toml.text                 "genesis_file"                 .= _config_genesisFile
  <*> Toml.table rpcCodec       "rpc"                          .= _config_rpc
  <*> Toml.table p2pCodec       "p2p"                          .= _config_p2p
  <*> Toml.table consensusCodec "consensus"                    .= _config_consensus

rpcCodec :: TomlCodec ConfigRPC
rpcCodec = pure ConfigRPC
  <*> Toml.text                 "laddr"                        .= _configRPC_laddr

p2pCodec :: TomlCodec ConfigP2P
p2pCodec = pure ConfigP2P
  <*> Toml.text                 "laddr"                        .= _configP2P_laddr
  <*> Toml.text                 "persistent_peers"             .= _configP2P_persistentPeers
  <*> Toml.text                 "private_peer_ids"             .= _configP2P_privatePeerIds
  <*> Toml.bool                 "addr_book_strict"             .= _configP2P_addrBookStrict
  <*> Toml.bool                 "allow_duplicate_ip"           .= _configP2P_allowDuplicateIp

consensusCodec :: TomlCodec ConfigConsensus
consensusCodec = pure ConfigConsensus
  <*> Toml.text                 "create_empty_blocks_interval" .= _configConsensus_createEmptyBlocksInterval

concat <$> traverse makeLenses
  [ ''Config
  , ''ConfigRPC
  , ''ConfigP2P
  , ''ConfigConsensus
  ]
