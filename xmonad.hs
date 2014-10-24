import XMonad
import XMonad.Util.EZConfig(additionalKeys)

main = do
        keyLayout <- spawn "setxkbmap -layout gb"
        conky     <- spawn "conky" --should probably check conky is running first
	xmonad $ defaultConfig
                   {
	             terminal    = "gnome-terminal",
	             modMask     = mod4Mask,
	             borderWidth = 2,
	             focusedBorderColor = "purple"
	           } `additionalKeys`
                       [((0, 0x1008FF12), spawn "amixer set Master toggle"),
                        ((0, 0x1008FF11), spawn "amixer set Master 2-"),
                        ((0, 0x1008FF13), spawn "amixer set Master 2+")
                       ]

