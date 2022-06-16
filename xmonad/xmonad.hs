--------------------------------------------------------------------------------

{- | Heavily modified XMonad config derived from the Example.hs file kindly
   supplied by the maintainers.
   Author: Adam McCullough
-}
module Main (main) where

--------------------------------------------------------------------------------
import Data.Tree
import GHC.IO.Handle.Types (Handle)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import XMonad
    ( Choose,
      Full (..),
      ManageHook,
      Resize (..),
      Tall (..),
      Window,
      X,
      XConfig (..),
      className,
      doFloat,
      doIgnore,
      io,
      logHook,
      mod4Mask,
      resource,
      sendMessage,
      spawn,
      whenJust,
      windows,
      withFocused,
      xmonad,
      (<+>),
      (<||>),
      (=?),
      (|||),
    )
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.EasyMotion (selectWindow)
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.Search
    ( SearchEngine,
      multi,
      namedEngine,
      promptSearch,
      searchEngine,
      (!>),
    )
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.Submap (submap)
import XMonad.Actions.TreeSelect
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Config.Kde
    ( desktopLayoutModifiers,
      kde4Config,
    )
import XMonad.Hooks.DynamicLog
    ( dynamicLogString,
      dynamicLogWithPP,
      pad,
      ppCurrent,
      ppHidden,
      ppHiddenNoWindows,
      ppLayout,
      ppOrder,
      ppOutput,
      ppSep,
      ppTitle,
      ppWsSep,
      shorten,
      xmobarColor,
      xmobarPP,
      xmonadPropLog,
    )
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks
    ( AvoidStruts,
      ToggleStruts (..),
      avoidStruts,
      docks,
    )
import XMonad.Hooks.ManageHelpers
    ( composeOne,
      doCenterFloat,
      isDialog,
      isKDETrayWindow,
      transience,
      (-?>),
    )
-- import XMonad.Layout.GapsModified ()

-- nsHideOnFocusLoss,

import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.WorkspaceHistory
import XMonad.Layout.Gaps
    ( Direction2D (..),
      GapMessage (..),
      Gaps,
      gaps',
    )
import XMonad.Layout.Grid (Grid (..))
import XMonad.Layout.LayoutModifier (ModifiedLayout (..))
import XMonad.Layout.NoBorders (SmartBorder, smartBorders)
import XMonad.Layout.Spacing
    ( Border (..),
      Spacing,
      spacingRaw,
      toggleScreenSpacingEnabled,
      toggleWindowSpacingEnabled,
    )
import XMonad.Layout.ThreeColumns (ThreeCol (..))
import XMonad.Layout.ToggleLayouts (ToggleLayout (..))
import XMonad.Layout.WindowNavigation (Navigate (..), WindowNavigation, windowNavigation)
import XMonad.Prompt (XPConfig (..), XPPosition (..))
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Unicode (unicodePrompt)
import XMonad.Prompt.XMonad (xmonadPrompt)
import qualified XMonad.StackSet as W
    ( RationalRect (..),
      float,
      focusWindow,
      greedyView,
      shift,
      swapMaster,
    )
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap, mkKeymap)
import XMonad.Util.NamedScratchpad
    ( NamedScratchpad (..),
      defaultFloating,
      namedScratchpadAction,
      namedScratchpadManageHook,
      nsHideOnFocusLoss,
    )
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)

--------------------------------------------------------------------------------
main :: IO ()
main = do
    dzenLeftBar <- spawnPipe "xmobar --dock"
    -- Start xmonad using the main desktop configuration with a few
    -- simple overrides:
    xmonad $
        docks $
            ewmh
                myXConfig
                    { logHook = myXmobarLogHook dzenLeftBar
                    }

myColorBG :: String
myColorBG = "#151515"

myColorWhite :: String
myColorWhite = "#ebebeb"

myColorRed :: String
myColorRed = "#C3143B"

myColorGray :: String
myColorGray = "#545454"

--myColorDarkgray :: String
--myColorDarkgray = "#353535"

term :: String
term = "kitty"

myXmobarLogHook :: GHC.IO.Handle.Types.Handle -> X ()
myXmobarLogHook xmproc =
    dynamicLogWithPP $
        xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppCurrent = xmobarColor myColorWhite myColorRed . pad
            , ppHidden = xmobarColor myColorWhite myColorBG . noScratchPad
            , ppHiddenNoWindows = xmobarColor myColorGray myColorBG . noScratchPad
            , ppSep = xmobarColor myColorRed myColorBG " | "
            , ppWsSep = xmobarColor myColorRed myColorBG ""
            , ppTitle = xmobarColor myColorWhite myColorBG . shorten 80
            , ppOrder = \(ws : l : t : _) -> [ws, l, t]
            , ppLayout = xmobarColor myColorWhite myColorBG
            }
    where
        noScratchPad ws = if ws == "NSP" then "" else pad ws

myXConfig :: XConfig (ModifiedLayout AvoidStruts LayoutType)
myXConfig =
    ewmhFullscreen $
        kde4Config
            { modMask = mod4Mask -- Use the "Win" key for the mod key
            , terminal = term
            , manageHook = myManageHook <+> manageHook desktopConfig <+> namedScratchpadManageHook myNamedScratchpads
            , handleEventHook = handleEventHook def
            , layoutHook = desktopLayoutModifiers myLayouts
            , logHook =
                dynamicLogString def >>= xmonadPropLog
                    >> workspaceHistoryHook
                    >> refocusLastLogHook
                    -- does not work right for some reason?
                    >> nsHideOnFocusLoss myNamedScratchpads
            , startupHook = myStartupHook
            , XMonad.workspaces = toWorkspaces myTreeWorkspaces
            }
            `additionalKeysP` myKeys

myTreeWorkspaces :: Forest String
myTreeWorkspaces =
    [ Node "Browser" [] -- a workspace for your browser
    , Node
        "Home" -- for everyday activity's
        [ Node "1" [] --  with 4 extra sub-workspaces, for even more activity's
        , Node "2" []
        , Node "3" []
        , Node "4" []
        ]
    , Node
        "Programming" -- for all your programming needs
        [ Node "Haskell" []
        , Node "Docs" [] -- documentation
        ]
    ]

github :: SearchEngine
github = searchEngine "github" "http://github.com/search?q="

myMulti :: SearchEngine
myMulti = namedEngine "Omni" $ github !> multi

myNamedScratchpads :: [NamedScratchpad]
myNamedScratchpads =
    [ NS "htop" "kitty htop -c xmonad_htop" (className =? "xmonad_htop") defaultFloating
    , NS "free42dec" "free42dec" (className =? "free42dec") defaultFloating
    , NS "nethogs" "kdesu kitty nethogs -c nethogs" (className =? "nethogs") defaultFloating
    , NS "ksysguard" "ksysguard" (className =? "ksysguard") defaultFloating
    , NS "volume" "pavucontrol" (className =? "pavucontrol") defaultFloating
    , NS "slashtime" "slashtime" (className =? "slashtime" <||> className =? "Slashtime") defaultFloating
    ]

myKeys :: [(String, X ())]
myKeys =
    [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess)) -- Add some extra key bindings:
    , ("M-c", shellPrompt myXPConfig)
    , ("M-S-c", namedScratchpadAction myNamedScratchpads "free42dec")
    , ("M-g", promptSearch myXPConfig myMulti)
    ,
        ( "M-m"
        , submap . mkKeymap myXConfig $
            [ ("h", namedScratchpadAction myNamedScratchpads "htop")
            , ("n", namedScratchpadAction myNamedScratchpads "nethogs")
            ]
        )
    , ("M-o", selectWindow def >>= (`whenJust` windows . W.focusWindow))
    , ("M-S-v", namedScratchpadAction myNamedScratchpads "volume")
    , ("M-<Backspace>", spawn "/usr/libexec/kscreenlocker_greet")
    , ("M-<Return>", spawn term)
    , ("M-S-<Esc>", namedScratchpadAction myNamedScratchpads "ksysguard")
    , ("M-S-s", namedScratchpadAction myNamedScratchpads "slashtime")
    , ("M-S-<Return>", spawn "emacs")
    , ("M-S-;", xmonadPrompt myXPConfig)
    , ("M-S-l", sendMessage Expand)
    , ("M-S-t", sinkAll)
    , ("M-<Esc>", sendMessage (Toggle "Full"))
    , ("M-w", treeselectWorkspace (def TSConfig) myTreeWorkspaces W.greedyView)
    , ("M-S-w", treeselectWorkspace (def TSConfig) myTreeWorkspaces W.shift)
    ,
        ( "M-u"
        , submap . mkKeymap myXConfig $
            [ ("b", withFocused toggleBorder)
            , ("M-c", shellPrompt myFuzzyXPConfig)
            , ("g", sendMessage ToggleGaps)
            , ("z", sendMessage ToggleStruts)
            , ("S-g", toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
            , ("q", kill1)
            , ("e", unicodePrompt "/home/merlin/UnicodeData.txt" myXPConfig)
            , ("s", windows W.swapMaster)
            , ("S-s", spawnOnce "spectacle")
            , ("t", spawn "tzclock")
            , ("f", withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1))
            ]
        )
    , ("M-<Up>", sendMessage $ Go U)
    , ("M-<Down>", sendMessage $ Go D)
    , ("M-<Left>", sendMessage $ Go L)
    , ("M-<Right>", sendMessage $ Go R)
    ]

--------------------------------------------------------------------------------

type LayoutType =
    ModifiedLayout
        Spacing
        ( ModifiedLayout
            AvoidStruts
            ( ModifiedLayout
                SmartBorder
                ( ModifiedLayout
                    WindowNavigation
                    ( Choose
                        (ModifiedLayout Gaps Grid)
                        ( Choose
                            (ModifiedLayout Gaps Full)
                            ( Choose
                                (ModifiedLayout Gaps Tall)
                                ( Choose
                                    (ModifiedLayout Gaps Tall)
                                    ( Choose
                                        (ModifiedLayout Gaps ThreeCol)
                                        (ModifiedLayout Gaps ThreeCol)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )

myLayouts :: LayoutType Window
myLayouts =
    spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
        avoidStruts $
            smartBorders $
                windowNavigation $
                    gap (GridRatio (4/3))
                        ||| gap Full
                        ||| gap (Tall nmaster delta ratio)
                        ||| gap (Tall (nmaster + 1) delta ratio)
                        ||| gap (ThreeCol nmaster delta ratio)
                        ||| gap (ThreeCol (nmaster + 1) delta ratio)
    where
        nmaster = 1
        delta = 3 / 100
        ratio = 1 / 2
        gap = gaps' [((L, 200), False), ((R, 200), False), ((U, 100), False), ((D, 100), False)]

--------------------------------------------------------------------------------

{- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
 great replacement for dzen.
-}
myXPConfig :: XPConfig
myXPConfig =
    def
        { alwaysHighlight = True
        , font = "xft:monospace:size=9"
        , position = Top
        , promptBorderWidth = 0
        }

myFuzzyXPConfig :: XPConfig
myFuzzyXPConfig = myXPConfig {searchPredicate = fuzzyMatch}

--------------------------------------------------------------------------------

{- | Manipulate windows as they are created.  The list given to
 @composeOne@ is processed from top to bottom.  The first matching
 rule wins.

 Use the `xprop' tool to get the info you need for these matches.
 For className, use the second value that xprop gives you.
-}
myManageHook :: ManageHook
myManageHook =
    composeOne
        [ className =? "plasma-desktop"
            <||> className =? "plasma"
            <||> className =? "krunner"
            <||> isDialog
            <||> isKDETrayWindow -?> doIgnore >> doFloat
        , className =? "zoom" -?> doFloat
        , className =? "XCalc" -?> doFloat
        , className =? "mpv" -?> doFloat
        , className =? "cairo-dock" -?> doIgnore >> doFloat
        , className =? "slashtime" <||> className =? "Slashtime" -?> doIgnore >> doFloat
        , isDialog -?> doCenterFloat
        , -- Move transient windows to their parent:
          transience
        , resource =? "stalonetray" -?> doIgnore
        ]

myStartupHook :: X ()
myStartupHook = do
    checkKeymap myXConfig myKeys
    spawnOnce "xcompmgr -c &"
    spawnOnce "telegram-desktop &"
    spawnOnce "signal &"
    spawnOnce "element &"
    spawnOnce "discord &"
    spawnOnce "zoom &"
    spawnOnce "slack &"
    spawnOnce "pkill cairo-dock && sleep 3 && cairo-dock &"
    spawnOnce "pkill redshift && sleep 3 && redshift -l geoclue2 &"
