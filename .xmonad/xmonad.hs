-- #Imports# --
import XMonad
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Layout.TwoPane
import XMonad.Actions.GridSelect

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Actions.CopyWindow
import Data.List (isInfixOf)


-- ## Startup ## ----------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "tint2 & nitrogen --restore & picom --config ~/.xmonad/picom.conf &"

-- ## Settings ## ---------------------------------------------------------------------
myTerminal = "ghostty"
myModMask = mod4Mask
myBorderWidth = 2
myFocusedBorderColor = "#26CCC2"
myNormalBorderColor  = "#272E33"

myWorkspaces = map show [1..5]

-- ## Layouts ## ----------------------------------------------------------------------
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

myGaps = gaps [(L,60),(R,0),(U,0),(D,0)]

myLayout = mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ mySpacing 4 $ myGaps $ tiled ||| Full ||| Mirror tiled
  where
    tiled = Tall 1 (3/100) (1/2)

-- ## Manage Hook ## ------------------------------------------------------------------
myManageHook = composeAll
    [ isDialog --> doCenterFloat
    , className =? "com.float.float" --> doRectFloat (W.RationalRect 0.2 0.2 0.6 0.6)
    , className =? "feh"  --> doCenterFloat
    , className =? "VirtualBox Manager" --> doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    , fmap ("Picture in picture" `isInfixOf`) title --> doRectFloat (W.RationalRect 0.70 0.70 0.28 0.28) <+> doF W.focusDown
    ]



-- ## Keybindings ## ------------------------------------------------------------------
myKeys =
    [ ("M-<Return>",              spawn myTerminal)
    , ("M-S-<Return>",            spawn "ghostty --class=com.float.float")
    , ("M-d",                     spawn "rofi -show drun")
    , ("M-C-t",              spawn myTerminal)
    , ("M-C-S-t",            spawn "ghostty --class=com.float.float")
    , ("M-a",                     spawn "bash ~/.xmonad/rofi/rofi_launcher")
    , ("M-w" ,                    spawn "rofi -show window")
    , ("M-x",                     spawn "bash ~/.xmonad/rofi/rofi_powermenu")
    , ("M-Space",                 withFocused toggleFloat)
    , ("M-f",                     sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)
    , ("M-C-g",                   sendMessage ToggleGaps)
    , ("M-C-<Space>",             sendMessage NextLayout)
    , ("M-c",                     kill)
    , ("M-q",                     spawn "xmonad --recompile && xmonad --restart")
    , ("M-=",                     sendMessage (IncGap 5 L)
                                  >> sendMessage (IncGap 5 R)
                                  >> sendMessage (IncGap 5 U)
                                  >> sendMessage (IncGap 5 D))
    , ("M--",                     sendMessage (DecGap 5 L)
                                  >> sendMessage (DecGap 5 R)
                                  >> sendMessage (DecGap 5 U)
                                  >> sendMessage (DecGap 5 D))
    , ("M-*",                     sendMessage $ ToggleGaps )
    , ("M-<Right>",               windows W.focusDown)
    , ("M-<Left>",                windows W.focusUp)
    , ("M-S-<Space>",             windows W.swapMaster)
    , ("M-S-<Right>",             windows W.swapDown)
    , ("M-S-<Left>",              windows W.swapUp)
    , ("<XF86AudioRaiseVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ("<XF86AudioLowerVolume>",  spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ("<XF86AudioMute>",         spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86MonBrightnessUp>",   spawn "brightnessctl s +4800")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl s 4800- -n 1")
    , ("M-s",                     windows copyToAll)
    ]

-- Float toggle helper
toggleFloat :: Window -> X ()
toggleFloat w = windows $ \s -> if M.member w (W.floating s)
                                   then W.sink w s
                                   else W.float w (W.RationalRect (1/4) (1/10) (1/2) (4/5)) s

-- ## Main ## -------------------------------------------------------------------------
main :: IO ()
main = xmonad $ ewmhFullscreen $ ewmh $ docks $ def 
    { terminal           = myTerminal
    , modMask            = myModMask
    , XMonad.borderWidth = myBorderWidth
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor  = myNormalBorderColor
    , workspaces         = myWorkspaces
    , manageHook         = myManageHook <+> manageHook def
    , layoutHook         = myLayout
    , startupHook        = myStartupHook
    , handleEventHook    = serverModeEventHookCmd
   }
    `additionalKeysP` myKeys
