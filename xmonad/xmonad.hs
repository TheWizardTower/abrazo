-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---INFORMATIONS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- informations = { Author   = Adam James McCullough
--                , Based on = Config by Graawr
--                , Version  = XMonad 0.15 <+> ghc lts 8.0.2 <+> xmobar 0.24.3
--                }

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---IMPORTS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
import           GHC.IO.Handle.Types                 (Handle)
import           System.Exit                         (exitSuccess)
import           System.IO                           (hPutStrLn)
import           XMonad                              hiding ((|||))
import qualified XMonad.Actions.ConstrainedResize    as Sqr
import           XMonad.Actions.CopyWindow           (kill1, runOrCopy)
import           XMonad.Actions.CycleWS              (WSType (..), moveTo,
                                                      nextScreen, prevScreen,
                                                      shiftNextScreen,
                                                      shiftPrevScreen)
import           XMonad.Actions.MouseResize
import           XMonad.Actions.NoBorders
import           XMonad.Actions.Submap
import           XMonad.Actions.Warp                 (warpToWindow)
import           XMonad.Actions.WindowGo             (raiseMaybe, runOrRaise)
import           XMonad.Actions.WithAll              (killAll, sinkAll)
import           XMonad.Config.Kde
import           XMonad.Hooks.DynamicLog             (PP (..), dynamicLogWithPP,
                                                      pad, shorten, xmobarColor,
                                                      xmobarPP)
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place
import           XMonad.Layout.Accordion
import           XMonad.Layout.Column
import           XMonad.Layout.Grid
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.MultiToggle           (Toggle (..))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed               (Rename (Replace), renamed)
import           XMonad.Layout.ThreeColumns
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle))
import           XMonad.Layout.WindowArranger
import           XMonad.Prompt                       (XPConfig (..),
                                                      XPPosition (Top))
import qualified XMonad.Prompt.Window                as WP
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet                     as W
import           XMonad.Util.EZConfig                (additionalKeysP,
                                                      additionalMouseBindings,
                                                      mkKeymap)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                     (runInTerm, safeSpawn,
                                                      spawnPipe)
import           XMonad.Util.Scratchpad              (scratchpadManageHook)
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WindowProperties        (getProp32s)

type NeoAwfulness =
    ModifiedLayout
        AvoidStruts
        ( NewSelect
            (ModifiedLayout Rename Grid)
            ( NewSelect
                (ModifiedLayout Rename Full)
                ( NewSelect
                    (ModifiedLayout Rename (Mirror Tall))
                    ( NewSelect
                        (ModifiedLayout Rename Tall)
                        ( NewSelect
                            (ModifiedLayout Rename Column)
                            ( NewSelect
                                (ModifiedLayout Rename Accordion)
                                ( NewSelect
                                    (ModifiedLayout Rename ThreeCol)
                                    (ModifiedLayout Rename (Mirror ThreeCol))
                                )
                            )
                        )
                    )
                )
            )
        )

type Awfulness = ModifiedLayout MouseResize (ModifiedLayout WindowArranger NeoAwfulness)

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---SETTINGS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Styles
myFont :: String
myFont = "-artwiz-snap-normal-*-normal-*-10-*-*-*-*-*-*-*"

myBorderWidth :: Dimension
myBorderWidth = 1

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

-- Settings

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "kitty"

myOtherTerminal :: String
myOtherTerminal = "cool-retro-term"

-- Prompts colors
myPromptConfig :: XPConfig
myPromptConfig =
    def
        { font = myFont
        , bgColor = myColorBG
        , fgColor = myColorRed
        , bgHLight = myColorBG
        , fgHLight = myColorWhite
        , borderColor = myColorBG
        , promptBorderWidth = myBorderWidth
        , height = 20
        , position = Top
        , historySize = 0
        }

--     -- Grid selector colors
-- myGridConfig :: Window -> Bool -> X (String, String)
-- myGridConfig = colorRangeFromClassName
--     (0x15,0x15,0x15) -- lowest inactive bg
--     (0x15,0x15,0x15) -- highest inactive bg
--     (0xC3,0x14,0x3B) -- active bg
--     (0x54,0x54,0x54) -- inactive fg
--     (0xEB,0xEB,0xEB) -- active fg

-- myGSConfig :: p -> GSConfig Window
-- myGSConfig _  = (buildDefaultGSConfig myGridConfig)
--     { gs_cellheight   = 65
--     , gs_cellwidth    = 120
--     , gs_cellpadding  = 10
--     , gs_font         = myFont
--     }

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---SCRATCHPADS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- If you need to find out X window properties, xprop is the tool you need.
myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "terminal" myOtherTerminal (className =? "cool-retro-term") myPosition
    , NS "music" "audacious" (className =? "Audacious") myPosition
    , NS "spotify" "/var/lib/snapd/snap/bin/spotify" (className =? "Spotify") myPosition
    , NS "rtorrent" "urxvtc_mod -name rtorrent -e rtorrent" (resource =? "rtorrent") myPosition
    , NS "calc" "free42dec" (role =? "Free42 Calculator") myPosition
    ]
  where
    myPosition = customFloating $ W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3)
    role = stringProperty "WM_WINDOW_ROLE"

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---KEYBINDINGS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myWorkspaceKeys :: [(String, X ())]
myWorkspaceKeys =
    [ (otherModMasks ++ "M-" ++ [key], action wsTag)
    | (wsTag, key) <- zip myWorkspaces "123456789"
    , (otherModMasks, action) <-
        [ ("", windows . W.view) -- was W.greedyView
        , ("S-", windows . W.shift)
        ]
    ]

myKeysPrime :: [(String, X ())]
myKeysPrime =
    -- Xmonad
    [ ("M-C-r", spawn "xmonad --recompile")
    , ("M-M1-r", spawn "xmonad --restart")
    , ("M-S-r", spawn "pkill xmobar && xmonad --restart")
    , ("M-M1-q", io exitSuccess)
    , ("M-<Backspace>", spawn "/usr/libexec/kscreenlocker_greet")
    , ("M-S-;", xmonadPrompt myPromptConfig)
    , -- Windows workaround. :sadface:
      -- Windows-L is captured by windows (it locks the screen). It's not easily remappable, so xmonad must change.
      -- We don't want to change the default binding (for when Linux is the host OS), but provide this for when I'm stuck in a VM.
      ("M-S-l", sendMessage $ Expand)
    , -- Windows
      ("M-r", refresh)
    , ("M-q", kill1)
    , ("M-C-q", killAll)
    , ("M-S-q", killAll >> moveTo Next nonNSP >> killAll >> moveTo Next nonNSP >> killAll >> moveTo Next nonNSP >> killAll >> moveTo Next nonNSP)
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-S-t", sinkAll)
    , ("M-s", windows W.swapMaster)
    , -- Layouts
      ("M-S-<Space>", sendMessage ToggleStruts)
    , ("M-d", asks (XMonad.layoutHook . config) >>= setLayout)
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-S-f", sendMessage (T.Toggle "float"))
    , ("M-S-g", sendMessage (T.Toggle "gimp"))
    , ("M-S-x", sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX)
    , ("M-S-y", sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY)
    , ("M-S-m", sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
    , ("M-S-b", sendMessage $ XMonad.Layout.MultiToggle.Toggle NOBORDERS)
    , ("M-S-d", sendMessage (XMonad.Layout.MultiToggle.Toggle NBFULL) >> sendMessage ToggleStruts)
    , ("M-g", withFocused toggleBorder)
    , -- Workspaces
      ("M-w", nextScreen)
    , ("M-e", prevScreen)
    , ("M-S-w", shiftNextScreen)
    , ("M-S-e", shiftPrevScreen)
    , -- Modal Bindings

        ( "M-u"
        , submap . mkKeymap myXConfig $
            [ ("c", spawn "krunner")
            , ("j", WP.windowPrompt myPromptConfig WP.Goto WP.allWindows)
            , ("M-<Return>", spawn "emacs")
            , ("<Backspace>", spawn "xscreensaver-command -lock")
            ,
                ( "l"
                , submap . mkKeymap myXConfig $
                    [ ("1", sendMessage $ JumpToLayout "1: Grid")
                    , ("2", sendMessage $ JumpToLayout "2: Full")
                    , ("3", sendMessage $ JumpToLayout "3: MirrorTiled")
                    , ("4", sendMessage $ JumpToLayout "4: Tiled")
                    , ("5", sendMessage $ JumpToLayout "5: Column1.6")
                    , ("6", sendMessage $ JumpToLayout "6: Accordion")
                    , ("7", sendMessage $ JumpToLayout "7: Three")
                    , ("8", sendMessage $ JumpToLayout "8: MirrorThree")
                    , -- Prompt to show the list of above layouts.

                        ( "l"
                        , xmonadPromptC
                            [ ("1: Full", sendMessage $ JumpToLayout "1: Grid")
                            , ("2: OneBig", sendMessage $ JumpToLayout "2: Full")
                            , ("3: MirrorTiled", sendMessage $ JumpToLayout "3: MirrorTiled")
                            , ("4: Tiled", sendMessage $ JumpToLayout "4: Tiled")
                            , ("5: Column1.6", sendMessage $ JumpToLayout "5: Column1.6")
                            , ("6: Accordion", sendMessage $ JumpToLayout "6: Accordion")
                            , ("7: Three", sendMessage $ JumpToLayout "7: Three")
                            , ("8: MirrorThree", sendMessage $ JumpToLayout "8: MirrorThree")
                            ]
                            myPromptConfig
                        )
                    ]
                )
            , ("q", kill1)
            , ("r", spawn "xmonad --recompile && pkill xmobar && xmonad --restart")
            , ("s", windows W.swapMaster)
            ,
                ( "u"
                , submap . mkKeymap myXConfig $
                    [ ("u", spawn "Xdialog --title  'Really, dude?' --screencenter --yesno 'Really, dude?' 10 30")
                    ]
                )
            ,
                ( "M-u"
                , submap . mkKeymap myXConfig $
                    [("M-<Return>", spawn "systemctl --user restart emacs")]
                )
            ]
        )
    , -- Apps

      ("M-<Return>", spawn myTerminal)
    , ("M-S-<Return>", spawn "emacsclient -c")
    , ("M-c", spawn "exe=`dmenu_run -nb '#151515' -nf '#545454' -sb '#C3143B' -sf '#ebebeb' -p 'run:' -i` && eval \"exec $exe\"")
    , ("M-f", raiseMaybe (runInTerm "-name ranger" "ranger") (resource =? "ranger"))
    , ("M-v", raiseMaybe (runInTerm "-name irssi" "irssi") (resource =? "irssi"))
    , ("M-o", raiseMaybe (runInTerm "-name htop" "htop") (resource =? "htop") >> warpToWindow (1 / 2) (1 / 2))
    , ("M-C-f", runOrCopy "dolphin" (resource =? "dolphin"))
    , ("M-C-<Return>", runOrRaise "trayerd" (resource =? "trayer"))
    , ("M-M1-f", runOrCopy "runInTerm  -name ranger -e ranger" (resource =? "ranger"))
    , ("M-M1-t", runOrCopy "runInTerm  -name newsbeuter -e newsbeuter" (resource =? "newsbeuter"))
    , ("M-M1-v", runOrCopy "runInTerm  -name irssi -e irssi" (resource =? "irssi"))
    , ("M-M1-o", runOrCopy "runInTerm  -name htop -e htop" (resource =? "htop") >> warpToWindow (1 / 2) (1 / 2))
    , ("M-C-M1-f", runOrRaise "thunar" (resource =? "thunar"))
    , -- Scratchpads
      ("M-M1-m", namedScratchpadAction myScratchpads "music")
    , ("M-C-m", namedScratchpadAction myScratchpads "spotify")
    , ("M-M1-c", namedScratchpadAction myScratchpads "calc")
    , ("M-M1-<Return>", namedScratchpadAction myScratchpads "terminal")
    , ("<XF86Tools>", namedScratchpadAction myScratchpads "music")
    , -- Multimedia Keys
      ("<XF86AudioPlay>", spawn "ncmpcpp toggle")
    , ("<XF86AudioPrev>", spawn "ncmpcpp prev")
    , ("<XF86AudioNext>", spawn "ncmpcpp next")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86HomePage>", safeSpawn "firefox" ["/home/logan/.config/infoconf.html"])
    , ("<XF86Search>", safeSpawn "google-chrome" ["https://www.duckduckgo.com/"])
    , ("<XF86Mail>", runOrRaise "icedove" (resource =? "icedove"))
    , ("<XF86Calculator>", runOrRaise "speedcrunch" (resource =? "speedcrunch"))
    , ("<XF86Eject>", spawn "toggleeject")
    , ("<Print>", spawn "spectacle")
    ]
  where
    nonNSP = WSIs (return (\ws -> W.tag ws /= "NSP"))

myKeys :: [(String, X ())]
myKeys = myWorkspaceKeys ++ myKeysPrime

-- nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

myMouseKeys :: [((KeyMask, Button), Window -> X ())]
myMouseKeys = [((mod4Mask .|. shiftMask, button3), \w -> XMonad.focus w >> Sqr.mouseResizeWindow w True)]

-- myModalKeys :: [a]
-- myModalKeys = [
--          ]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---WORKSPACES
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = ["1.text", "2.web", "3.media", "4.comms", "5.misc", "6", "7", "8", "9.syst"]

namedScratchpads :: [a]
namedScratchpads =
    []

myManageHook :: ManageHook
myManageHook =
    scratchpadManageHook (W.RationalRect l t w h)
        <+> ( composeAll $
                [ className =? "Yakuake" --> doFloat
                , className =? "Steam" --> doFloat
                , className =? "steam" --> doFloat
                , className =? "Pidgin" --> doShift "1.text"
                , className =? "Gimp" --> doShift "5.misc"
                ]
                    ++ [ className =? "Plasma-desktop" --> doFloat
                       , className =? "plasmashell" --> doFloat
                       , className =? "plasma-desktop" --> makeMaster <+> doFloat
                       , className =? "Plasma" --> makeMaster <+> doFloat
                       , className =? "plasma" --> makeMaster <+> doFloat
                       ]
                    ++ [ isFullscreen --> doFullFloat
                       , isDialog --> placeHook (inBounds (underMouse (0, 0))) <+> makeMaster <+> doFloat
                       ]
            )
        <+> namedScratchpadManageHook namedScratchpads
        <+> manageDocks
        <+> makeMaster
  where
    makeMaster = insertPosition Master Newer
    -- role = stringProperty "WM_WINDOW_ROLE"
    h = 0.4
    w = 0.75
    t = 0.85 - h
    l = 0.87 - w

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---LAYOUTS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mainLayout :: NeoAwfulness Window
mainLayout =
    avoidStruts $
        rename "1: Grid" Grid
            ||| rename "2: Full" Full
            ||| rename "3: MirrorTiled" (Mirror tiled)
            ||| rename "4: Tiled" (tiled)
            ||| rename "5: Column1.6" (Column 1.6)
            ||| rename "6: Accordion" (Accordion)
            ||| rename "7: Three" (three)
            ||| rename "8: MirrorThree" (Mirror three)
  where
    rename s = renamed [Replace s]
    -- Default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio

    three = ThreeCol 1 (3 / 100) (1 / 2)

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proporton of the screen occupied by the master pane
    ratio = 1 / 2

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

myLayout :: Awfulness Window
myLayout =
    mouseResize $
        windowArrange mainLayout

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---STATUSBAR
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--- myXmonadBarR = "conky -c /home/logan/.xmonad/statusbar/conky_dzen | dzen2 -x '1000' -y '24' -w '680' -h '16' -ta 'r' -bg '"++myColorBG++"' -fg '"++myColorWhite++"' -fn '"++myFont++"'"

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

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---AUTOSTART
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "xsetroot -cursor_name left_ptr &"
    spawnOnce "unclutter &"
    spawnOnce "compton -bc -t -8 -l -9 -r 6 -o 0.7 -m 1.0 &"
    spawnOnce "xcompmgr -c &"
    spawnOnce "xmodmap ~/.Xmodmap &"
    spawnOnce "telegram &"
    spawnOnce "signal &"
    spawnOnce "element &"
    spawnOnce "discord &"
    spawnOnce "zoom &"
    spawnOnce "slack &"
    spawnOnce "systemctl --user start emacs"
    spawnOnce "pkill  redshift && sleep 3 && redshift -l geoclue2 &"
    docksStartupHook

kdeOverride :: Query Bool
kdeOverride =
    ask >>= \w -> liftX $ do
        override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
        wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
        return $ maybe False (elem $ fromIntegral override) wt

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---CONFIG
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myXConfig :: XConfig (Awfulness)
myXConfig =
    kde4Config
        { modMask = myModMask
        , terminal = myTerminal
        , manageHook =
            ( ( className =? "krunner" <||> className
                    =? "Plasma-desktop"
              )
                >>= return . not
                --> manageHook kde4Config
            )
                <+> (kdeOverride --> doFloat)
                <+> myManageHook
        , layoutHook = myLayout
        , startupHook = myStartupHook
        , XMonad.workspaces = myWorkspaces
        , borderWidth = myBorderWidth
        , normalBorderColor = myColorDarkgray
        , focusedBorderColor = myColorWhite
        }
        `additionalKeysP` myKeys
        `additionalMouseBindings` myMouseKeys

main :: IO ()
main = do
    dzenLeftBar <- spawnPipe "xmobar --dock"
    xmonad $
        myXConfig
            { logHook = myXmobarLogHook dzenLeftBar
            }
