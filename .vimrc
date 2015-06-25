set nocompatible
filetype off
set backspace=indent,eol,start
syntax on
set number
set hidden
"Set fonts for guis"
if has("gui_running")
	:set guioptions-=T  "remove toolbar	
	if has("gui_gtk2")
		set guifont=Ubuntu\ Mono\ 13
	elseif has("gui_macvim")
		set guifont=Ubuntu\ Mono:h16
	endif
endif 

set history=100
set wildmode=longest,list,full
set wildmenu
set shell=/bin/zsh
set vb "visualbell"
set ai "autoindent"
set ls=2 "show status line with only 1 window"

"Configure Vundle and plugins you want it to install"
"To use this run `git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim && vim +PluginInstall +qall`"
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'reedes/vim-colors-pencil'
Plugin 'bling/vim-airline'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'scrooloose/syntastic'
Plugin 'nikolavp/vim-jape'
Plugin 'kien/ctrlp.vim'

call vundle#end()

filetype plugin indent on

" Use dark pencil theme "
colorscheme pencil
set background=dark
let g:airline_theme='powerlineish'

" Easy motion config "
nmap s <Plug>(easymotion-s)
let g:EasyMotion_smartcase = 1

" Syntastic config "
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0 

" Keyboard shortcuts for ctrlp "
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

command! JsonPPrint :%! python -m json.tool
