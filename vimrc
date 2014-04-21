set nocompatible 
filetype off
let mapleader=" "

" {{{ Vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle "Raimondi/delimitMate"
Bundle "bling/vim-airline"
Bundle "kien/ctrlp.vim"
Bundle "majutsushi/tagbar"
Bundle "skammer/vim-css-color"
Bundle "tomasr/molokai"
Bundle "tpope/vim-endwise"
Bundle "tpope/vim-markdown"
Bundle "tpope/vim-rails"
Bundle "tpope/vim-vinegar"
" }}}
" {{{ Basic Settings
filetype plugin indent on

set cursorline
set expandtab "spaces instead of tabs
set foldmethod=marker
set noerrorbells
set number
set omnifunc=syntaxcomplete#Complete
set shiftwidth=4 "4 spaces for indentation
set tabstop=4 "4 spaces per tab
set title
set vb t_vb=

let g:netrw_liststyle=3 "display files as a tree

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
colorscheme molokai
set background=dark
set colorcolumn=80
set guifont=Inconsolata\ for\ Powerline\ 12
set nowrap
set ruler
set t_Co=256
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

