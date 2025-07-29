module UI.GhcDebug where

import Brick (BrickEvent (..), EventM, attrMap, get, on)
import Brick.BChan (newBChan)
import Brick.Main (App (..), customMainWithDefaultVty, halt, showFirstCursor)
import Control.Exception (bracket, handle)
import Control.Monad.IO.Class (liftIO)
import GHC.Debug.Brick.Lib (debuggeeConnect, pause, resume, version)
import GHC.Debug.Brick.Model (
  AppState (..),
  ConnectedMode (PausedMode),
  Event,
  FooterMode (FooterInfo),
  MajorState (Connected, _debuggee, _debuggeeSocket, _mode),
  Name,
  OperationalState (OperationalState),
  OverlayMode (NoOverlay),
  RootsOrigin (DefaultRoots),
  keybindingsMode,
  mkSocketInfo,
 )
import GHC.Debug.Brick.UI
import Graphics.Vty qualified as Vty
import Lens.Micro.Platform (view)

app :: App AppState Event Name
app =
  App
    { appDraw = myAppDraw
    , appChooseCursor = showFirstCursor
    , appHandleEvent = captureEsc
    , appStartEvent = myAppStartEvent
    , appAttrMap =
        const $
          attrMap
            Vty.defAttr
            [ (menuAttr, Vty.defAttr)
            , (inputAttr, Vty.brightWhite `on` Vty.blue)
            , (labelAttr, Vty.withStyle Vty.defAttr Vty.bold)
            , (highlightAttr, Vty.brightWhite `on` Vty.blue)
            , (treeAttr, Vty.withStyle Vty.defAttr Vty.dim)
            , (disabledMenuAttr, Vty.withStyle Vty.defAttr Vty.dim)
            ]
    }

captureEsc :: BrickEvent Name Event -> EventM Name AppState ()
captureEsc evt = do
  appState <- get
  case (appState, evt) of
    (AppState (Connected _ debuggee' (PausedMode os)) _, VtyEvent (Vty.EvKey Vty.KEsc [])) | NoOverlay <- view keybindingsMode os -> do
      liftIO $ resume debuggee'
      halt
    _ -> myAppHandleEvent evt

debug :: String -> IO ()
debug socketPath = handle @IOError (\_ -> pure ()) $ do
  socket <- mkSocketInfo socketPath
  eventChan <- newBChan 10
  bracket
    (liftIO $ debuggeeConnect (const $ pure ()) socketPath)
    (\debuggee' -> liftIO $ resume debuggee')
    (\debuggee' -> do
      liftIO $ pause debuggee'
      ver <- liftIO $ version debuggee'
      (rootsTree, initRoots) <- liftIO $ mkSavedAndGCRootsIOTree debuggee'
      let appState =
            AppState
              { _majorState =
                  Connected
                    { _debuggeeSocket = socket
                    , _debuggee = debuggee'
                    , _mode =
                        PausedMode
                          ( OperationalState
                              Nothing
                              Nothing
                              savedAndGCRoots
                              NoOverlay
                              FooterInfo
                              (DefaultRoots initRoots)
                              rootsTree
                              eventChan
                              (Just 100)
                              []
                              ver
                          )
                    }
              , _appChan = eventChan
              }
      (_, vty) <- customMainWithDefaultVty (Just eventChan) app appState
      Vty.shutdown vty
    )