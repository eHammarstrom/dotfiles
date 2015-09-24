" VimRC iniShip

filetype off

call plug#begin('~/.vim/plugged')

" Theme
Plug 'chriskempson/base16-vim'

" Utilities
" Plug 'powerline/powerline'
" Plug 'bling/vim-airline'
Plug 'scrooloose/syntastic'
Plug 'ervandew/supertab'

" Language specifics
Plug 'octol/vim-cpp-enhanced-highlight'

call plug#end()

filetype plugin indent on

" This enables powerline
set laststatus=2
