set nocompatible
filetype off
set backspace=indent,eol,start
syntax on
set number
set hidden
set guifont=Ubuntu\ Mono:h15
set history=100
set wildmode=longest,list,full
set wildmenu
set shell=/bin/zsh
set vb "visualbell"
set ai "autoindent"
set ls=2 "show status line with only 1 window"

"Configure Vundle and plugins you want it to install"
"To use this run `git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim` && vim +PluginInstall +qall`"
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'reedes/vim-colors-pencil'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-fireplace'
call vundle#end()

filetype plugin indent on

" Use dark pencil theme "
colorscheme pencil
set background=dark
let g:airline_theme='powerlineish'