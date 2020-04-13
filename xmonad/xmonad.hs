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
    -- Base
import           Data.Map                            as M
import           Data.Maybe                          (isJust)
import           System.Exit                         (exitSuccess)
import           System.IO                           (hPutStrLn)
import           XMonad                              hiding ((|||))
import           XMonad.Actions.NoBorders
import           XMonad.Actions.Submap
import           XMonad.Config.Kde
import           XMonad.ManageHook
import qualified XMonad.StackSet                     as W

    -- Utilities
import           XMonad.Util.EZConfig                (additionalKeysP,
                                                      additionalMouseBindings,
                                                      mkKeymap)
-- import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadManageHook, namedScratchpadAction, customFloating)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                     (runInTerm, safeSpawn,
                                                      spawnPipe, unsafeSpawn)
import           XMonad.Util.Scratchpad              (scratchpadFilterOutWorkspace,
                                                      scratchpadManageHook,
                                                      scratchpadSpawnAction)
import           XMonad.Util.SpawnOnce
import           XMonad.Util.WindowProperties        (getProp32s)

    -- Hooks
import           XMonad.Hooks.DynamicLog             (PP (..), defaultPP,
                                                      dynamicLogWithPP,
                                                      dzenColor, pad, shorten,
                                                      wrap, xmobarColor,
                                                      xmobarPP, xmobarStrip)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FloatNext              (floatNextHook,
                                                      toggleFloatAllNew,
                                                      toggleFloatNext)
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place

    -- Actions
import qualified XMonad.Actions.ConstrainedResize    as Sqr
import           XMonad.Actions.CopyWindow           (copyToAll, kill1,
                                                      killAllOtherCopies,
                                                      runOrCopy)
import           XMonad.Actions.CycleWS              (WSType (..), moveTo,
                                                      nextScreen, prevScreen,
                                                      shiftNextScreen,
                                                      shiftPrevScreen, shiftTo)
import           XMonad.Actions.DynamicWorkspaces    (addWorkspacePrompt,
                                                      removeEmptyWorkspace)
import           XMonad.Actions.GridSelect           (GSConfig (..),
                                                      bringSelected,
                                                      buildDefaultGSConfig,
                                                      colorRangeFromClassName,
                                                      goToSelected)
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves            (rotAllDown, rotSlavesDown)
import           XMonad.Actions.Warp                 (Corner (LowerRight),
                                                      banishScreen,
                                                      warpToWindow)
import           XMonad.Actions.WindowGo             (raiseMaybe, runOrRaise)
import           XMonad.Actions.WithAll              (killAll, sinkAll)

    -- Layouts modifiers
import           XMonad.Layout.BoringWindows         (boringWindows)
import           XMonad.Layout.LimitWindows          (decreaseLimit,
                                                      increaseLimit,
                                                      limitWindows)
import           XMonad.Layout.Maximize
import           XMonad.Layout.Minimize
import           XMonad.Layout.MultiToggle           (EOT (EOT), Toggle (..),
                                                      mkToggle, single, (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.PerWorkspace          (onWorkspace)
import           XMonad.Layout.Reflect               (REFLECTX (..),
                                                      REFLECTY (..),
                                                      reflectHoriz, reflectVert)
import           XMonad.Layout.Renamed               (Rename (CutWordsLeft, Replace),
                                                      renamed)
import           XMonad.Layout.Spacing               (spacing)
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle),
                                                           toggleLayouts)
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (..),
                                                      windowArrange)
import           XMonad.Layout.WorkspaceDir

    -- Layouts
import           XMonad.Layout.Accordion
import           XMonad.Layout.Column
import           XMonad.Layout.Grid
import           XMonad.Layout.IM                    (Property (Role), withIM)
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OneBig
import           XMonad.Layout.Reflect
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.ZoomRow               (ZoomMessage (ZoomFullToggle),
                                                      zoomIn, zoomOut,
                                                      zoomReset, zoomRow)

    -- Prompts
import           XMonad.Prompt                       (Direction1D (..),
                                                      XPConfig (..),
                                                      XPPosition (Top), def,
                                                      defaultXPConfig)
import qualified XMonad.Prompt.Window                as WP
import           XMonad.Prompt.XMonad


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---SETTINGS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    -- Styles
myFont          = "-artwiz-snap-normal-*-normal-*-10-*-*-*-*-*-*-*"
myBorderWidth   = 1
myColorBG       = "#151515"
myColorWhite    = "#ebebeb"
myColorRed      = "#C3143B"
myColorGray     = "#545454"
myColorDarkgray = "#353535"

    -- Settings
myModMask       = mod4Mask
myTerminal      = "konsole"

    -- Prompts colors
myPromptConfig =
    defaultXPConfig { font                  = myFont
                    , bgColor               = myColorBG
                    , fgColor               = myColorRed
                    , bgHLight              = myColorBG
                    , fgHLight              = myColorWhite
                    , borderColor           = myColorBG
                    , promptBorderWidth     = myBorderWidth
                    , height                = 20
                    , position              = Top
                    , historySize           = 0
                    }

    -- Grid selector colors
myGridConfig = colorRangeFromClassName
    (0x15,0x15,0x15) -- lowest inactive bg
    (0x15,0x15,0x15) -- highest inactive bg
    (0xC3,0x14,0x3B) -- active bg
    (0x54,0x54,0x54) -- inactive fg
    (0xEB,0xEB,0xEB) -- active fg

myGSConfig colorizer  = (buildDefaultGSConfig myGridConfig)
    { gs_cellheight   = 65
    , gs_cellwidth    = 120
    , gs_cellpadding  = 10
    , gs_font         = myFont
    }


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---SCRATCHPADS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- If you need to find out X window properties, xprop is the tool you need.
myScratchpads =
              [ NS "terminal"        "konsole"                                      (className =? "Konsole")        myPosition
              , NS "music"           "audacious"                                    (className =? "Audacious")      myPosition
              , NS "spotify"         "/var/lib/snapd/snap/bin/spotify"              (className =? "Spotify")      myPosition
              , NS "rtorrent"        "urxvtc_mod -name rtorrent -e rtorrent"        (resource =? "rtorrent")        myPosition
              , NS "calc"            "free42dec"                                    (role =? "Free42 Calculator")   myPosition
              ] where
                myPosition = customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3)
                role = stringProperty "WM_WINDOW_ROLE"


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---KEYBINDINGS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myKeys =
    -- Xmonad
        [ ("M-C-r",             spawn "xmonad --recompile")
        , ("M-M1-r",            spawn "xmonad --restart")
        , ("M-S-r",             spawn "pkill xmobar && xmonad --restart")
        , ("M-M1-q",            io exitSuccess)
        , ("M-<Backspace>",     spawn "/usr/libexec/kscreenlocker_greet")
        , ("M-S-;",               xmonadPrompt myPromptConfig)
    -- Windows workaround. :sadface:
        -- Windows-L is captured by windows (it locks the screen). It's not easily remappable, so xmonad must change.
        -- We don't want to change the default binding (for when Linux is the host OS), but provide this for when I'm stuck in a VM.
        , ("M-S-l",             sendMessage $ Expand)

    -- Windows
        , ("M-r",               refresh)
        , ("M-q",               kill1)
        , ("M-C-q",             killAll)
        , ("M-S-q",             killAll >> moveTo Next nonNSP >> killAll >> moveTo Next nonNSP >> killAll >> moveTo Next nonNSP >> killAll >> moveTo Next nonNSP)

        , ("M-t",               withFocused $ windows . W.sink)
        , ("M-S-t",             sinkAll)
        , ("M-s",               windows W.swapMaster)


    -- Layouts
        , ("M-S-<Space>",       sendMessage ToggleStruts)
        , ("M-d",               asks (XMonad.layoutHook . config) >>= setLayout)
        , ("M-<Space>",         sendMessage NextLayout)
        , ("M-S-f",             sendMessage (T.Toggle "float"))
        , ("M-S-g",             sendMessage (T.Toggle "gimp"))
        , ("M-S-x",             sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTX)
        , ("M-S-y",             sendMessage $ XMonad.Layout.MultiToggle.Toggle REFLECTY)
        , ("M-S-m",             sendMessage $ XMonad.Layout.MultiToggle.Toggle MIRROR)
        , ("M-S-b",             sendMessage $ XMonad.Layout.MultiToggle.Toggle NOBORDERS)
        , ("M-S-d",             sendMessage (XMonad.Layout.MultiToggle.Toggle NBFULL) >> sendMessage ToggleStruts)
        , ("M-g",               withFocused toggleBorder)

    -- Workspaces
        , ("M-w",               nextScreen)
        , ("M-e",               prevScreen)
        , ("M-S-w",             shiftNextScreen)
        , ("M-S-e",             shiftPrevScreen)

    -- Modal Bindings
        ,  ("M-u",   submap . mkKeymap myXConfig $
           [("c", spawn "krunner")
         , ("j", WP.windowPromptGoto def)
         , ("M-<Return>",    spawn "emacs")
         , ("<Backspace>",   spawn "xscreensaver-command -lock")
         , ("l", submap .  mkKeymap myXConfig $
                 [  ("1", sendMessage $ JumpToLayout "1: Full")
                 ,  ("2", sendMessage $ JumpToLayout "2: OneBig")
                 ,  ("3", sendMessage $ JumpToLayout "3: MirrorTiled")
                 ,  ("4", sendMessage $ JumpToLayout "4: Tiled")
                 ,  ("5", sendMessage $ JumpToLayout "5: Column1.6")
                 ,  ("6", sendMessage $ JumpToLayout "6: Accordion")
                 ,  ("7", sendMessage $ JumpToLayout "7: Three")
                 ,  ("8", sendMessage $ JumpToLayout "8: MirrorThree")
                 -- Prompt to show the list of above layouts.
                 ,  ("l", xmonadPromptC [  ("1: Full", sendMessage $ JumpToLayout "1: Full")
                                        ,  ("2: OneBig", sendMessage $ JumpToLayout "2: OneBig")
                                        ,  ("3: MirrorTiled", sendMessage $ JumpToLayout "3: MirrorTiled")
                                        ,  ("4: Tiled", sendMessage $ JumpToLayout "4: Tiled")
                                        ,  ("5: Column1.6", sendMessage $ JumpToLayout "5: Column1.6")
                                        ,  ("6: Accordion", sendMessage $ JumpToLayout "6: Accordion")
                                        ,  ("7: Three", sendMessage $ JumpToLayout "7: Three")
                                        ,  ("8: MirrorThree", sendMessage $ JumpToLayout "8: MirrorThree")
                                        ] myPromptConfig)
                 ])
         , ("q", kill1)
         , ("r", spawn "xmonad --recompile && pkill xmobar && xmonad --restart")
         , ("s", windows W.swapMaster)
         , ("u", submap . mkKeymap myXConfig $
                 [("u", spawn "Xdialog --titlle  'Really, dude?' --screencenter --yesno 'Really, dude?' 10 30")
                 ])
         , ("M-u", submap . mkKeymap myXConfig $
             [("M-<Return>",    spawn "systemctl --user restart emacs")])
         ])

    -- Apps

        , ("M-<Return>",        spawn "konsole")
        , ("M-S-<Return>",      spawn "emacsclient -c")
        , ("M-c",               spawn "exe=`dmenu_run -nb '#151515' -nf '#545454' -sb '#C3143B' -sf '#ebebeb' -p 'run:' -i` && eval \"exec $exe\"")
        , ("M-f",               raiseMaybe (runInTerm "-name ranger" "ranger") (resource =? "ranger"))
        , ("M-v",               raiseMaybe (runInTerm "-name irssi" "irssi") (resource =? "irssi"))
        , ("M-o",               raiseMaybe (runInTerm "-name htop" "htop") (resource =? "htop") >> warpToWindow (1/2) (1/2))
        , ("M-C-f",             runOrCopy "dolphin" (resource =? "dolphin"))
        , ("M-C-<Return>",      runOrRaise "trayerd" (resource =? "trayer"))
        , ("M-M1-f",            runOrCopy "runInTerm  -name ranger -e ranger" (resource =? "ranger"))
        , ("M-M1-t",            runOrCopy "runInTerm  -name newsbeuter -e newsbeuter" (resource =? "newsbeuter"))
        , ("M-M1-v",            runOrCopy "runInTerm  -name irssi -e irssi" (resource =? "irssi"))
        , ("M-M1-o",            runOrCopy "runInTerm  -name htop -e htop" (resource =? "htop") >> warpToWindow (1/2) (1/2))
        , ("M-C-M1-f",          runOrRaise "thunar" (resource =? "thunar"))


    -- Scratchpads
        , ("M-M1-m",               namedScratchpadAction myScratchpads "music" )
        , ("M-C-m",                namedScratchpadAction myScratchpads "spotify" )
        , ("M-M1-c",               namedScratchpadAction myScratchpads "calc" )
        , ("M-M1-<Return>",        namedScratchpadAction myScratchpads "terminal" )
        , ("<XF86Tools>",          namedScratchpadAction myScratchpads "music")

    -- Multimedia Keys
        , ("<XF86AudioPlay>",   spawn "ncmpcpp toggle")
        , ("<XF86AudioPrev>",   spawn "ncmpcpp prev")
        , ("<XF86AudioNext>",   spawn "ncmpcpp next")
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>",    safeSpawn "firefox" ["/home/logan/.config/infoconf.html"])
        , ("<XF86Search>",      safeSpawn "google-chrome" ["https://www.duckduckgo.com/"])
        , ("<XF86Mail>",        runOrRaise "icedove" (resource =? "icedove"))
        , ("<XF86Calculator>",  runOrRaise "speedcrunch" (resource =? "speedcrunch"))
        , ("<XF86Eject>",       spawn "toggleeject")
        , ("<Print>",           spawn "spectacle")
        ] where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))

myMouseKeys = [ ((mod4Mask .|. shiftMask, button3), \w -> focus w >> Sqr.mouseResizeWindow w True) ]


myModalKeys = [
         ]


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---WORKSPACES
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myWorkspaces = ["1.text", "2.web", "3.media", "4.comms", "5.misc", "6", "7", "8", "9.syst"]

namedScratchpads =
           [
           ]

myManageHook = scratchpadManageHook (W.RationalRect l t w h) <+>
               (composeAll $
         [  className =? "Yakuake"           --> doFloat
          , className =? "Steam"             --> doFloat
          , className =? "steam"             --> doFloat
          , className =? "Pidgin"            --> doShift "1.text"
          , className =? "Gimp"              --> doShift "5.misc"
         ]
         ++
         [  className =? "Plasma-desktop" --> doFloat
          , className =? "plasmashell"    --> doFloat
          , className =? "plasma-desktop" --> makeMaster <+> doFloat
          , className =? "Plasma"         --> makeMaster <+> doFloat
          , className =? "plasma"         --> makeMaster <+> doFloat
         ]
         ++
         [   isFullscreen --> doFullFloat
           , isDialog     --> placeHook (inBounds (underMouse (0,0))) <+> makeMaster <+> doFloat

         ])
         <+> namedScratchpadManageHook namedScratchpads
         <+> manageDocks
         <+> makeMaster
         where
           makeMaster = insertPosition Master Newer
           role = stringProperty "WM_WINDOW_ROLE"
           h = 0.4
           w = 0.75
           t = 0.85 - h
           l = 0.87 - w



-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---LAYOUTS
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mainLayout = avoidStruts $ rename "1: Full" Full
             ||| rename "2: OneBig" (OneBig (3/4) (3/4))
             ||| rename "3: MirrorTiled" ( Mirror tiled)
             ||| rename "4: Tiled" (tiled)
             ||| rename "5: Column1.6" (Column 1.6)
             ||| rename "6: Accordion" (Accordion)
             ||| rename "7: Three" (three)
             ||| rename "8: MirrorThree" (Mirror three)
   where
     rename s = renamed [Replace s]
     -- Default tiling algorithm partitions the screen into two panes
     tiled = Tall nmaster delta ratio

     three = ThreeCol 1 (3/100) (1/2)

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proporton of the screen occupied by the master pane
     ratio = 1/2

     -- Percent of screen to increment by when resizing panes
     delta = 3/100

myLayout = mouseResize $
           windowArrange mainLayout

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---STATUSBAR
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--- myXmonadBarR = "conky -c /home/logan/.xmonad/statusbar/conky_dzen | dzen2 -x '1000' -y '24' -w '680' -h '16' -ta 'r' -bg '"++myColorBG++"' -fg '"++myColorWhite++"' -fn '"++myFont++"'"

myXmobarLogHook xmproc = dynamicLogWithPP $ xmobarPP
                      { ppOutput          = hPutStrLn xmproc
                      , ppCurrent         = xmobarColor myColorWhite myColorRed . pad
                      , ppHidden          = xmobarColor myColorWhite myColorBG . noScratchPad
                      , ppHiddenNoWindows = xmobarColor myColorGray myColorBG . noScratchPad
                      , ppSep             = xmobarColor myColorRed myColorBG " | "
                      , ppWsSep           = xmobarColor myColorRed myColorBG ""
                      , ppTitle           = xmobarColor myColorWhite myColorBG . shorten 80
                      , ppOrder           = \(ws:l:t:_) -> [ws,l,t]
                      , ppLayout          = xmobarColor myColorWhite myColorBG
                      } where noScratchPad ws = if ws == "NSP" then "" else pad ws


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---AUTOSTART
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myStartupHook = do
          spawnOnce "xsetroot -cursor_name left_ptr &"
          spawnOnce "unclutter &"
          spawnOnce "compton -bc -t -8 -l -9 -r 6 -o 0.7 -m 1.0 &"
          spawnOnce "xcompmgr -c &"
          spawnOnce "redshift -l geoclue2 &"
          spawnOnce "xmodmap ~/.Xmodmap &"
          spawnOnce "systemctl --user start emacs"
          docksStartupHook

kdeOverride :: Query Bool
kdeOverride = ask >>= \w -> liftX $ do
    override <- getAtom "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
    wt <- getProp32s "_NET_WM_WINDOW_TYPE" w
    return $ maybe False (elem $ fromIntegral override) wt

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---CONFIG
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myXConfig = kde4Config
        { modMask            = myModMask
        , terminal           = myTerminal
        , manageHook         = ((className =? "krunner" <||> className =?
 "Plasma-desktop") >>= return . not --> manageHook kde4Config) <+>
 (kdeOverride --> doFloat) <+> myManageHook
        , layoutHook         = myLayout
        , startupHook        = myStartupHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myColorDarkgray
        , focusedBorderColor = myColorWhite
        } `additionalKeysP`         myKeys
          `additionalMouseBindings` myMouseKeys




main = do
    dzenLeftBar <- spawnPipe "xmobar --dock"
    xmonad       $ myXConfig {
      logHook = myXmobarLogHook dzenLeftBar
                }
