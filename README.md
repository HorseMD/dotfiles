#Dotfiles

Simply a place to dump my dotfiles.

##Required Packages

###For Xmonad

* Xmonad
* Xmonad-contrib
* Xmobar

###Others

* Emacs
* GVim / Vim

##Installation

Make sure you have the relevant programs installed (Emacs, xmobar, etc..) before
running `install.sh`.

1. Run `git clone https://github.com/HorseMD/dotfiles.git` to download these dotfiles.
2. Mark `install.sh` as executable (`chmod +x install.sh`).
3. Run `install.sh`.
4. When prompted, choose whether you want the desktop version of xmobarrc or the laptop version.

A backup of any existing dotfiles will be placed in the `dotfiles_bkp` folder which will be
created in the same directory by `install.sh`, should it need to.

##TODO

1. Maybe make `install.sh` check for installed programs (Emacs, etc...)
