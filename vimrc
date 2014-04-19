set nocompatible 
filetype off
let mapleader=" "

" {{{ Vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle "bling/vim-airline"
Bundle "kien/ctrlp.vim"
Bundle "tpope/vim-markdown"
Bundle "tpope/vim-endwise"
Bundle "tomasr/molokai"
Bundle "majutsushi/tagbar"
Bundle "skammer/vim-css-color"
Bundle "Raimondi/delimitMate"
Bundle "tpope/vim-vinegar"
" }}}
" {{{ Basic Settings
filetype plugin indent on

set omnifunc=syntaxcomplete#Complete
set foldmethod=marker
set number
set cursorline
set noerrorbells
set vb t_vb=
set title
set expandtab "spaces instead of tabs
set tabstop=4 "4 spaces per tab
set shiftwidth=4 "4 spaces for indentation

let g:netrw_liststyle=3

set nobackup
set noswapfile
set nowb
" {{{ Filetype-specific Settings
augroup filetype_autocommands
    autocmd!
    autocmd Filetype ruby setlocal ts=2 sts=2 sw=2
augroup END
" }}}
" }}}
" {{{ Aesthetics
set t_Co=256
colorscheme molokai
set background=dark
set ruler
set colorcolumn=80
set guifont=Inconsolata\ for\ Powerline\ 12
set nowrap
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
" }}}
" {{{ Functions
" {{{ Which tags generator?
function! TagGen()
    let tagmethod = "ctags"
    if &filetype == "ruby"
        silent !ripper-tags -R
        let tagmethod = "ripper-tags"
    else
        silent !ctags -R
    endif
    redraw!
    echomsg "Generated tags with " . tagmethod
endfunction
" }}}
command! Tags call TagGen()
" }}}
" {{{ Mappings
" uppercase-ify the current word, keeps cursor at end of word.
inoremap <C-u> <esc>viwUea
" open vimrc in a split and go to the bottom.
nnoremap <leader>ev :vsplit $MYVIMRC<Cr>G
" surround the visual selection in quotes, keeps cursor at end of selection.
vnoremap <leader>" <esc>`>a"<esc>`<i"<esc>f"
" }}}

