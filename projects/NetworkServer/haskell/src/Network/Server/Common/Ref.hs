module Network.Server.Common.Ref where

import Network.Server.Common.HandleLens(HandleLens(..))
import Network.Server.Common.Lens(iso)
import System.IO(Handle)
import Data.Function(on)

-- https://hackage.haskell.org/package/base-4.9.1.0/docs/System-IO.html#t:Handle

newtype Ref =
  Ref Handle
  deriving (Eq, Show)

instance Ord Ref where
  compare =
    compare `on` show

instance HandleLens Ref where
  handleL =
    iso (\(Ref h) -> h) Ref
