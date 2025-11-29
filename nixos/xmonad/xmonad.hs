-- Clean, modern XMonad configuration
-- Includes EWMH, proper fullscreen, gaps, spacing, floating rules,
-- and tidy keybindings.

-- ## Imports ## ----------------------------------------------------------------------
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

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- ## Startup ## ----------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "~/.xmonad/autorun.sh"

-- ## Settings ## ---------------------------------------------------------------------
myTerminal = "ghostty"
myModMask = mod4Mask
myBorderWidth = 2
myFocusedBorderColor = "#0000FF"
myNormalBorderColor  = "#272E33"

myWorkspaces = map show [1..10]

-- ## Layouts ## ----------------------------------------------------------------------
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

myGaps = gaps [(L,60),(R,0),(U,0),(D,30)]

myLayout = mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ mySpacing 4 $ myGaps $ Full ||| tiled ||| Mirror tiled
  where
    tiled = Tall 1 (3/100) (1/2)

-- ## Manage Hook ## ------------------------------------------------------------------
myManageHook = composeAll
    [ isDialog --> doCenterFloat
    , className =? "Gimp" --> doCenterFloat
    , className =? "feh"  --> doCenterFloat
    , className =? "VirtualBox Manager" --> doCenterFloat
    , resource  =? "desktop_window" --> doIgnore
    ]

-- ## Keybindings ## ------------------------------------------------------------------
myKeys =
    [ ("M-<Return>", spawn myTerminal)
    , ("M-d", spawn "rofi -show drun")
    -- Floating / fullscreen
    , ("M-Space", withFocused toggleFloat)
    , ("M-f", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)

    -- Gaps
    , ("M-C-g", sendMessage ToggleGaps)

    -- Layout switching
    , ("M-C-<Space>", sendMessage NextLayout)

    -- Windows
    , ("M-c", kill)

    -- Restart XMonad
    , ("M-q", spawn "xmonad --recompile && xmonad --restart")

    -- Increase
    , ("M-=", sendMessage (IncGap 5 L)
                 >> sendMessage (IncGap 5 R)
                 >> sendMessage (IncGap 5 U)
                 >> sendMessage (IncGap 5 D))

    -- Decrease
    , ("M--", sendMessage (DecGap 5 L)
                 >> sendMessage (DecGap 5 R)
                 >> sendMessage (DecGap 5 U)
                 >> sendMessage (DecGap 5 D))
    , ("M-*", sendMessage $ ToggleGaps )

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
    , borderWidth        = myBorderWidth
    , focusedBorderColor = myFocusedBorderColor
    , normalBorderColor  = myNormalBorderColor
    , workspaces         = myWorkspaces
    , manageHook         = myManageHook <+> manageHook def
    , layoutHook         = myLayout
    , startupHook        = myStartupHook
    }
    `additionalKeysP` myKeys

