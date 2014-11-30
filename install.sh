#!/bin/bash

# This should be changed dotfiles was cloned outside the root of your home directory!
dotfiles=~/dotfiles
# base directory to place backups in
backup_dir=$dotfiles/dotfiles_bkp

# Create the folder if it doesn't exist. Also echo this.
# $1 = file/path/here
make_backupdir() {
    if [ ! -d $1 ]; then
	echo "* Creating backup directory $1"
	mkdir -p $1
    fi
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
    echo "Linked $dotfiles/$1 to ~/.$1\n"
}

for dotfile in vimrc bashrc emacs.d/init.el emacs.d/daniel.org xmonad/xmonad.hs; do
    setup_dotfile $dotfile
done

echo "Done."
