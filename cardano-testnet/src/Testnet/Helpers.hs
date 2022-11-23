module Testnet.Helpers where

import Prelude
import           Control.Monad (when)
import qualified Data.List as L

import qualified System.Info as OS
import qualified System.Process as IO

import qualified Hedgehog.Extras.Test.Base as H
-- import qualified Hedgehog.Extras.Test.Concurrent as H
-- import qualified Hedgehog.Extras.Test.File as H
-- import qualified Hedgehog.Extras.Test.Network as H
-- import qualified Hedgehog.Extras.Test.Process as H

-- * Helpers

noteListeningPort :: String -> H.Integration ()
noteListeningPort portString = when (OS.os `L.elem` ["darwin", "linux"]) $ do
  H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""
