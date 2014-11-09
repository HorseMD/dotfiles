set nocompatible
filetype off
let mapleader=" "

" {{{ Vundle
set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

Plugin 'Raimondi/delimitMate'
Plugin 'bling/vim-airline'
Plugin 'godlygeek/tabular'
Plugin 'kien/ctrlp.vim'
Plugin 'majutsushi/tagbar'
Plugin 'rpdelaney/vim-sourcecfg'
Plugin 'skammer/vim-css-color'
Plugin 'tomasr/molokai'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-markdown'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-vinegar'

call vundle#end()
" }}}
" {{{ Basic Settings
filetype plugin indent on

set cursorline
set foldmethod=marker
set noerrorbells
set number
set omnifunc=syntaxcomplete#Complete
set title
set vb t_vb=
set wildmenu

set expandtab "spaces instead of tabs
set shiftwidth=4 "4 spaces for indentation
set tabstop=4 "4 spaces per tab
set autoindent

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
set colorcolumn=80
set nowrap
set ruler
" {{{ Conditional
if system("tput colors") == 256 || has("gui_running")
    set t_Co=256
    colorscheme molokai
    set background=dark
else
    echom "This terminal doesn't support a 256 colorscheme!"
endif

if has("gui_running")
    set guifont=Inconsolata\ for\ Powerline\ 12
endif
" }}}
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
" {{{ Convert this markdown file to HTML
function! MarkdownToHTML()
    silent !python -m markdown % > index.html
    redraw!
endfunction
" }}}
command! Tags call TagGen()
command! ToHTML call MarkdownToHTML()
" }}}
" {{{ Mappings
" uppercase-ify the current word, keeps cursor at end of word.
inoremap <C-u> <esc>viwUea
" open vimrc in a split and go to the bottom.
nnoremap <leader>ev :vsplit $MYVIMRC<Cr>G
" surround the visual selection in quotes, keeps cursor at end of selection.
vnoremap <leader>" <esc>`>a"<esc>`<i"<esc>f"
" create a fold out of the selection, with the first line as the header.
vnoremap <leader>c <esc>`>a<CR>//}}}<esc>`<wwi{{{ <esc>zc
" <Leader>o opens CtrlP
nnoremap <leader>o :CtrlP<cr>
" <Leader>g shows the highlighting groups the item under the cursor is.
nnoremap <leader>g :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
            \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
            \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
" <Leader>t aligns the visual selection by <first word> <rest of line>
vnoremap <leader>t :Tabularize /^\s*\S\+\zs/l0c1l0<CR>
" }}}
" {{{ Events
" Remove trailing spaces when the file is saved.
autocmd BufWritePre * :%s/\s\+$//e
" }}}
