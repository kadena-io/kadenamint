{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kadenamint.Tendermint.Config where

import Control.Arrow (left, (>>>))
import Control.Lens (makeLenses)
import Data.Text    (Text)
import qualified Data.Text as T
import GHC.Generics
import Text.URI (URI, mkURI, render)

import Toml (AnyValue, BiMap(..), TomlBiMap, TomlBiMapError(ArbitraryError), TomlCodec, _Text, match, (.=))
import qualified Toml

import Kadenamint.Common

data Config = Config
  { _config_moniker :: Text
  , _config_proxyApp :: URI
  , _config_fastSync :: Bool
  , _config_genesisFile :: Text
  , _config_rpc :: ConfigRPC
  , _config_p2p :: ConfigP2P
  , _config_consensus :: ConfigConsensus
  } deriving (Eq, Ord, Show, Generic)

data ConfigRPC = ConfigRPC
  { _configRPC_laddr :: URI
  } deriving (Eq, Ord, Show, Generic)

data ConfigP2P = ConfigP2P
  { _configP2P_laddr :: URI
  , _configP2P_persistentPeers :: Text
  , _configP2P_privatePeerIds :: Text
  , _configP2P_addrBookStrict :: Bool
  , _configP2P_allowDuplicateIp :: Bool
  } deriving (Eq, Ord, Show, Generic)

data ConfigConsensus = ConfigConsensus
  { _configConsensus_createEmptyBlocksInterval :: Text
  } deriving (Eq, Ord, Read, Show, Generic)


uri :: Toml.Key -> TomlCodec URI
uri = match _URI

_URI :: TomlBiMap URI AnyValue
_URI = _URI2Text >>> _Text

_URI2Text :: TomlBiMap URI Text
_URI2Text = BiMap
    { forward  = Right . removeTrailingSlash . render
    , backward = left (ArbitraryError . tshow) . mkURI
    }
  where
    removeTrailingSlash = todo Todo_Upstream $ T.reverse . T.dropWhile (== '/') . T.reverse

configCodec :: TomlCodec Config
configCodec = pure Config
  <*> Toml.text                 "moniker"                      .= _config_moniker
  <*> uri                       "proxy_app"                    .= _config_proxyApp
  <*> Toml.bool                 "fast_sync"                    .= _config_fastSync
  <*> Toml.text                 "genesis_file"                 .= _config_genesisFile
  <*> Toml.table rpcCodec       "rpc"                          .= _config_rpc
  <*> Toml.table p2pCodec       "p2p"                          .= _config_p2p
  <*> Toml.table consensusCodec "consensus"                    .= _config_consensus

rpcCodec :: TomlCodec ConfigRPC
rpcCodec = pure ConfigRPC
  <*> uri                       "laddr"                        .= _configRPC_laddr

p2pCodec :: TomlCodec ConfigP2P
p2pCodec = pure ConfigP2P
  <*> uri                       "laddr"                        .= _configP2P_laddr
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
