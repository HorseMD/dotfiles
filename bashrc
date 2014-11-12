# General Settings
export EDITOR="emacs -nw"
export GREP_OPTIONS="--color=auto"
export LANG="en_GB.UTF-8"
export PS1="\[\e[00;33m\]\u \[\e[0m\]\[\e[00;37m\]at \[\e[0m\]\[\e[00;35m\]\h \[\e[00;37m\]in \W\[\e[0m\]: "

if [ -d ~/.cabal/bin ]; then
   export PATH=$PATH:~/.cabal/bin
fi

shopt -s histappend
export HISTCONTROL=ignoredups
export HISTFILESIZE=10000
export HISTIGNORE="ls*:clear:exit"
export HISTSIZE=1000

if [ -n "$DISPLAY" -a "$TERM" == "xterm" ]; then
    export TERM=xterm-256color
fi

# Aliases
alias ls="ls --color=auto"
