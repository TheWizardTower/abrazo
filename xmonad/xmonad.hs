--------------------------------------------------------------------------------

{- | Heavily modified XMonad config derived from the Example.hs file kindly
   supplied by the maintainers.
  Author: Adam McCullough
-}

{- | Example.hs

 Example configuration file for xmonad using the latest recommended
 features (e.g., 'desktopConfig').
-}
module Main (main) where

--------------------------------------------------------------------------------
import           GHC.IO.Handle.Types                (Handle)
import           System.Exit
import           System.IO                          (hPutStrLn)
import           XMonad
import           XMonad.Actions.CopyWindow          (kill1)
import           XMonad.Actions.Search
import           XMonad.Actions.SinkAll             (sinkAll)
import           XMonad.Actions.Submap              (submap)
import           XMonad.Config.Desktop
import           XMonad.Config.Kde
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.BinarySpacePartition (emptyBSP)
import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders            (noBorders)
import           XMonad.Layout.ResizableTile        (ResizableTall (..))
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.ToggleLayouts        (ToggleLayout (..),
                                                     toggleLayouts)
import           XMonad.Prompt                      (XPConfig (..),
                                                     XPPosition (..))
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Unicode
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet                    as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                    (runInTerm, safeSpawn,
                                                     spawnPipe)
import           XMonad.Util.SpawnOnce

--------------------------------------------------------------------------------
main = do
    dzenLeftBar <- spawnPipe "xmobar --dock"
    -- Start xmonad using the main desktop configuration with a few
    -- simple overrides:
    xmonad $
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

myColorDarkgray :: String
myColorDarkgray = "#353535"

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
    kde4Config
        { modMask = mod4Mask -- Use the "Win" key for the mod key
        , terminal = term
        , manageHook = myManageHook <+> (manageHook desktopConfig) <+> (namedScratchpadManageHook myNamedScratchpads)
        , handleEventHook = handleEventHook def <+> fullscreenEventHook
        , layoutHook = desktopLayoutModifiers $ myLayouts
        , logHook = dynamicLogString def >>= xmonadPropLog
        , startupHook = myStartupHook
        , XMonad.workspaces = myWorkspaces
        }
        `additionalKeysP` myKeys

myWorkspaces :: [String]
myWorkspaces = ["1.text", "2.web", "3.media", "4.comms", "5.misc", "6", "7", "8", "9.syst"]

myNamedScratchpads :: [NamedScratchpad]
myNamedScratchpads =
    [ NS "htop" "kitty htop -c xmonad_htop" (className =? "xmonad_htop") defaultFloating
    , NS "free42dec" "free42dec" (className =? "free42dec") defaultFloating
    , NS "nethogs" "kdesu kitty nethogs -c nethogs" (className =? "nethogs") defaultFloating
    , NS "ksysguard" "ksysguard" (className =? "ksysguard") defaultFloating
    , NS "volume" "pavucontrol" (className =? "pavucontrol") defaultFloating
    ]

myKeys :: [(String, X ())]
myKeys =
    [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess)) -- Add some extra key bindings:
    , ("M-c", shellPrompt myXPConfig)
    , ("M-S-c", namedScratchpadAction myNamedScratchpads "free42dec")
    , ("M-g", promptSearch myXPConfig multi)
    ,
        ( "M-m"
        , submap . mkKeymap myXConfig $
            [ ("h", namedScratchpadAction myNamedScratchpads "htop")
            , ("n", namedScratchpadAction myNamedScratchpads "nethogs")
            ]
        )
    , ("M-n", namedScratchpadAction myNamedScratchpads "nethogs")
    , ("M-S-v", namedScratchpadAction myNamedScratchpads "volume")
    , ("M-<Backspace>", spawn "/usr/libexec/kscreenlocker_greet")
    , ("M-<Return>", spawn term)
    , ("M-S-<Esc>", namedScratchpadAction myNamedScratchpads "ksysguard")
    , ("M-S-<Return>", spawn "emacs")
    , ("M-S-;", xmonadPrompt myXPConfig)
    , ("M-S-l", sendMessage $ Expand)
    , ("M-S-t", sinkAll)
    , ("M-<Esc>", sendMessage (Toggle "Full"))
    ,
        ( "M-u"
        , submap . mkKeymap myXConfig $
            [ ("g", sendMessage ToggleGaps)
            , ("z", sendMessage ToggleStruts)
            , ("S-g", toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
            , ("q", kill1)
            , ("e", unicodePrompt "/home/merlin/UnicodeData.txt" myXPConfig)
            , ("s", windows W.swapMaster)
            ]
        )
    ]

--------------------------------------------------------------------------------

type LayoutType =
    ModifiedLayout
        Spacing
        ( ModifiedLayout
            AvoidStruts
            ( Choose
                (ModifiedLayout Gaps Grid)
                ( Choose
                    (ModifiedLayout Gaps Full)
                    (Choose (ModifiedLayout Gaps Tall) (Choose (ModifiedLayout Gaps Tall) (Choose (ModifiedLayout Gaps ThreeCol) (ModifiedLayout Gaps ThreeCol))))
                )
            )
        )

myLayouts :: LayoutType Window
myLayouts =
    spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
        avoidStruts $
            (gap $ Grid)
                ||| (gap $ Full)
                ||| (gap $ Tall nmaster delta ratio)
                ||| (gap $ Tall (nmaster + 1) delta ratio)
                ||| (gap $ ThreeCol nmaster delta ratio)
                ||| (gap $ ThreeCol (nmaster + 1) delta ratio)
    where
        nmaster = 1
        delta = (3 / 100)
        ratio = (1 / 2)
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
        , searchPredicate = fuzzyMatch
        -- , sorter = fuzzySort
        }

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
        [ className =? "plasma-desktop" <||> className =? "krunner" <||> isKDETrayWindow -?> doIgnore >> doFloat
        , className =? "XCalc" -?> doFloat
        , className =? "mpv" -?> doFloat
        , className =? "cairo-dock" -?> doIgnore >> doFloat
        , isDialog -?> doCenterFloat
        , -- Move transient windows to their parent:
          transience
        ]

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xcompmgr -c &"
    spawnOnce "telegram-desktop &"
    spawnOnce "signal &"
    spawnOnce "element &"
    spawnOnce "discord &"
    spawnOnce "zoom &"
    spawnOnce "slack &"
    spawnOnce "pkill cairo-dock && sleep 3 && cairo-dock &"
    spawnOnce "pkill redshift && sleep 3 && redshift -l geoclue2 &"
    docksStartupHook
