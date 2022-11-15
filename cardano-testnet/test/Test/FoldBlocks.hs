{-# LANGUAGE OverloadedStrings  #-}

module Test.FoldBlocks where

import Prelude
import System.FilePath ((</>))
import qualified System.Directory as IO
import qualified Control.Concurrent as IO
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as TS
import           Control.Concurrent.Async (async, link)
import           Control.Exception (Exception, throw)

import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test as HE
import qualified Hedgehog.Extras.Stock.IO.Network.Sprocket as HE
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty (TestTree, testGroup)

import qualified Cardano.Api as C
import qualified Testnet.Cardano as TN
import qualified Testnet.Conf as TC (Conf (..), ProjectBase (ProjectBase), YamlFilePath (YamlFilePath), mkConf)
import qualified Util.Base as U
import qualified Util.Runtime as U

-- workspace_
import qualified System.IO.Temp as IO
import qualified System.Info as IO
import qualified System.Environment as IO
import qualified GHC.Stack as GHC
import           Control.Monad (when)

import Test.Tasty (defaultMain)


newtype FoldBlocksException = FoldBlocksException C.FoldBlocksError
instance Exception FoldBlocksException
instance Show FoldBlocksException where
  show (FoldBlocksException a) = TS.unpack $ C.renderFoldBlocksError a

tests :: TestTree
tests = testGroup "FoldBlocks"
  [ testPropertyNamed "foldBlocks receives ledger state" "prop_foldBlocks_fails" prop_foldBlocks
  ]

-- | This test starts a testnet with wery short timing, then starts
-- foldBlocks in another thread to listen for ledger state, ledger
-- events and block, and on reception writes this to an MVar that main
-- thread blocks on.
prop_foldBlocks :: H.Property
prop_foldBlocks = U.integration . H.runFinallies . workspace_ "chairman" $ \tempAbsBasePath' -> do

  -- Start testnet
  base <- HE.noteM $ liftIO . IO.canonicalizePath =<< HE.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf <- HE.noteShowM $
    TC.mkConf (TC.ProjectBase base) (TC.YamlFilePath configurationTemplate)
      (tempAbsBasePath' <> "/")
      Nothing

  let options = TN.defaultTestnetOptions
        -- Set opoch to 2 slot, slot to 0.1 seconds
        { TN.epochLength = 8
        , TN.slotLength = 0.2
--        , TN.bftNodeOptions = [TN.defaultTestnetNodeOptions]
        }
  runtime <- TN.cardanoTestnet options conf

  -- Get socketPath
  socketPathAbs <- do
    socketPath' <- HE.sprocketArgumentName <$> HE.headM (U.nodeSprocket <$> TN.bftNodes runtime)
    H.note =<< liftIO (IO.canonicalizePath $ TC.tempAbsPath conf </> socketPath')

  configurationFile <- H.noteShow $ TC.tempAbsPath conf </> "configuration.yaml"

  -- Start foldBlocks in a separate thread
  lock <- liftIO IO.newEmptyMVar
  liftIO $ do
    a <- async $ do
      let handler _env _ledgerState _ledgerEvents _blockInCardanoMode _ = IO.putMVar lock ()
      e <- runExceptT (C.foldBlocksNonPipelined configurationFile socketPathAbs  C.QuickValidation () handler)
      either (throw . FoldBlocksException) (\_ -> pure ()) e
      putStrLn "foldBlocks has finished"
    link a -- Throw foldBlocks threads' exceptions in main thread.

  _ <- liftIO $ IO.readMVar lock
  H.assert True

-- | Use this instead of H.workspace, which on Cicero gives exception
--
-- Exception (SomeAsyncException) 2022-11-14T15:41:20.687206742+00:00
-- stderr F cardano-testnet-test-cardano-testnet-tests>
-- ExceptionInLinkedThread (ThreadId 425) pokeSockAddr: path is too
-- long
workspace_ :: (H.MonadTest m, MonadIO m, GHC.HasCallStack) => FilePath -> (FilePath -> m ()) -> m ()
workspace_ prefixPath f = GHC.withFrozenCallStack $ do
  liftIO $ putStrLn $ "OS is " <> IO.os
  systemTemp <- case IO.os of
    "darwin" -> pure "/tmp"
    _        -> H.evalIO IO.getCanonicalTemporaryDirectory
  maybeKeepWorkspace <- H.evalIO $ IO.lookupEnv "KEEP_WORKSPACE"
  let systemPrefixPath = systemTemp <> "/" <> prefixPath
  H.evalIO $ IO.createDirectoryIfMissing True systemPrefixPath
  ws <- H.evalIO $ IO.createTempDirectory systemPrefixPath "test"
  H.annotate $ "Workspace: " <> ws
  -- liftIO $ IO.writeFile (ws <> "/module") callerModuleName
  f ws
  when (IO.os /= "mingw32" && maybeKeepWorkspace /= Just "1") $ do
    H.evalIO $ IO.removeDirectoryRecursive ws

hot :: IO ()
hot = defaultMain tests
