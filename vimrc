set nocompatible 
filetype off

" {{{ Vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle "bling/vim-airline"
Bundle "kien/ctrlp.vim"
Bundle "mattn/emmet-vim"
Bundle "tpope/vim-markdown"
Bundle "scrooloose/nerdtree"
Bundle "tpope/vim-endwise"
Bundle "withgod/vim-sourcepawn"
Bundle "tomasr/molokai"
" }}}
" {{{ Basic Settings
filetype plugin indent on

set omnifunc=syntaxcomplete#Complete

set cm=blowfish "more secture crypto

set foldmethod=marker
set number
set cursorline
set noerrorbells
set vb t_vb=
set title
set expandtab "spaces instead of tabs
set tabstop=4 "4 spaces per tab
set shiftwidth=4 "4 spaces for indentation

set nobackup
set noswapfile
set nowb
" }}}
" {{{ Aesthetics
set t_Co=256
colorscheme molokai
set background=dark
set ruler
set colorcolumn=80
set guifont=Inconsolata\ for\ Powerline\ 12
" }}}
" {{{ Plugin Settings
" {{{ vim-airline
set laststatus=2
let g:airline_powerline_fonts=1

" speedily update statusline when leaving Insert mode to Command mode
augroup FastEscape
    autocmd!
    au InsertEnter * set timeoutlen=0
    au InsertLeave * set timeoutlen=1000
augroup END
" }}}
" SourcePawn {{{
" quickfix
au FileType sourcepawn setlocal makeprg=/media/Storage/Gaming/Linux/hlserver/tf2/tf/addons/sourcemod/scripting/spcomp\ %
" }}}
" {{{ Filetype-specific Settings
autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
" }}}

