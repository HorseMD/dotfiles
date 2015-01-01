#!/bin/bash

# This should be changed if dotfiles was cloned outside the root of your home directory!
dotfiles=~/dotfiles
# base directory to place backups in
backup_dir=$dotfiles/dotfiles_bkp
# path to where xmobarrc will end up, most likely will never need to be changed.
xmobarfile=$dotfiles/xmonad/xmobarrc

# Create the folder if it doesn't exist. Also echo this.
# $1 = file/path/here
make_backupdir() {
    if [ ! -d $1 ]; then
	echo "* Creating backup directory $1"
	mkdir -p $1
    fi
}

# Given either desktop or laptop as a parameter, use the
# corresponding xmobarrc as this computer's...xmobarrc.
link_xmobarrc() {
    eval ln -sf $dotfiles/xmonad/xmobarrc-$1 $xmobarfile
    echo "* Using $1 version of xmobarrc."
}

# Given a file path, link its relevant location in $HOME
# $1 = folder/path/to/file
setup_dotfile() {
    echo "Linking $(basename $1):"

    # remove any symlink that's already there
    if [ -L ~/.$1 ]; then
	echo "* Removing old link to $dotfile"
	rm ~/.$1
    fi

    # backup any REAL file that's there
    if [ -f ~/.$1 ]; then
	echo "* File ~/.$1 already exists, backing up..."
	base_dir=$(dirname $1)
	make_backupdir $backup_dir/$base_dir
	mv ~/.$1 $backup_dir/$1
    fi

    ln -s $dotfiles/$1 ~/.$1
    echo "* Linked $dotfiles/$1 to ~/.$1\n"
}

for dotfile in vimrc bashrc emacs.d/init.el emacs.d/daniel.org xmonad/xmonad.hs; do
    setup_dotfile $dotfile
done

echo "Choose an xmobar configuration:"
opts=("Desktop" "Laptop" "Exit")
PS3="> "

echo $dotfiles/xmonad/xmobarrc-desktop

select opt in "${opts[@]}"; do
    case "$REPLY" in
        1) link_xmobarrc "desktop"; break;;
        2) link_xmobarrc "laptop";  break;;
        3) break;;
        *) echo "* Invalid option, please type 1, 2 or 3."; continue;;
    esac
done

echo "Recompiling xmonad.hs:"
xmonad --recompile

if [ $? -eq 0 ]; then
    echo "* Success."
else
    echo "* Couldn't recompile; is xmonad installed?"
fi

echo "Done."
