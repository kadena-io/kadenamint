{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Kadenamint.Tendermint
  ( module Kadenamint.Tendermint
  , module Kadenamint.Tendermint.Config
  ) where

import Control.Lens              (makeLenses)
import Data.Text                 (Text)
import GHC.Generics

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

mkNetworkFlags :: Text -> Int -> NetworkFlags
mkNetworkFlags networkRoot size = NetworkFlags
  { _networkFlags_validators    = size
  , _networkFlags_output        = networkRoot
  , _networkFlags_populatePeers = True
  }

concat <$> traverse makeLenses
  [ ''InitializedNode
  , ''GlobalFlags
  , ''NetworkFlags
  ]
