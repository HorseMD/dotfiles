import XMonad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import qualified XMonad.StackSet as W

myManageHook = composeAll
    [ (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)
    ]
  where role = stringProperty "WM_WINDOW_ROLE"

main = do
        keyLayout <- spawnPipe "setxkbmap -layout gb"
        mobar     <- spawnPipe "xmobar ~/dotfiles/xmonad/xmobarrc"

	xmonad $ defaultConfig
                   {
                     startupHook = setWMName "LG3D",
	             terminal    = "st",
	             modMask     = mod4Mask,
	             borderWidth = 2,
                     layoutHook  = avoidStruts $ layoutHook defaultConfig,
	             focusedBorderColor = "purple",
                     manageHook = myManageHook
	           } `additionalKeys`
                       [((0, 0x1008FF12), spawn "amixer set Master toggle"),
                        ((0, 0x1008FF11), spawn "amixer set Master 5-"),
                        ((0, 0x1008FF13), spawn "amixer set Master 5+"),
                        ((mod4Mask, xK_l), spawn "xscreensaver-command -lock")
                       ]

