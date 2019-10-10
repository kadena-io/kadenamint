{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

module Kadenamint.Common where

import Control.Concurrent                               (threadDelay)
import Control.Monad.IO.Class                           (MonadIO(..))
import Control.Monad.Reader                             (MonadReader(..), asks)
import Data.Colour.SRGB                                 (Colour, sRGB24)
import Data.String                                      (IsString(..))
import Data.Text                                        (Text)
import System.Console.ANSI                              (SGR(..), setSGRCode)
import qualified Data.Text as T

localhost :: Text
localhost = "127.0.0.1"

{- Logging -}
newtype Env = Env
  { _env_printer :: Text -> Text
  }

red, green, cyan :: Colour Float
red   = sRGB24 0xFF 0 0
green = sRGB24 0 0xFF 0
cyan  = sRGB24 0 0xFF 0xFF

log :: (MonadIO m, MonadReader Env m) => Text -> Maybe Text -> m ()
log header body = do
  p <- asks _env_printer
  liftIO $ putStrLn $ T.unpack $ p $ header <> maybe "" (\b -> ":\n" <> T.unlines (("  " <>) <$> T.lines b)) body

sgrify :: [SGR] -> Text -> Text
sgrify codes txt = mconcat
  [ T.pack $ setSGRCode codes
  , txt
  , T.pack $ setSGRCode [Reset]
  ]

{- Text -}
tshow :: Show a => a -> Text
tshow = T.pack . show

cleave :: Text -> Text -> (Text, Text)
cleave sep str =
  let (a,b) = T.breakOn sep str
  in (a, T.drop (T.length sep) b)

singleQuotes :: (IsString a, Semigroup a) => a -> a
singleQuotes t = "'" <> t <> "'"

doubleQuotes :: (IsString a, Semigroup a) => a -> a
doubleQuotes t = "\"" <> t <> "\""

{- Time -}
seconds :: Int -> Int
seconds = (*1e6)

sleep :: MonadIO m => Int -> m ()
sleep = liftIO . threadDelay . seconds

{- Errors -}
fatal :: a
fatal = error "fatal error"

impossible :: a
impossible = error "the 'impossible' has happened"

{- Issue tracking -}
_UPSTREAM_ :: Text -> a -> a
_UPSTREAM_ _ = id

_TODO_ :: Text -> a -> a
_TODO_ _ = id

_ASSUME_ :: Text -> a -> a
_ASSUME_ _ = id
