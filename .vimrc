" VimRC iniShip

filetype off

call plug#begin('~/.vim/plugged')

" Theme
Plug 'chriskempson/base16-vim'

" Utilities
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'ervandew/supertab'
Plug 'jiangmiao/auto-pairs'

" Language specifics
Plug 'vim-ruby/vim-ruby'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'tpope/vim-rails'
Plug 'klen/python-mode'

call plug#end()

filetype plugin indent on

set encoding=utf-8

" This enables powerline
set laststatus=2

set tabstop=8
set softtabstop=8
set shiftwidth=8
set noexpandtab

set number

" line wrapping
" set tw=80
" set fo=cqt
" set wm=0

" line wrapping test
" augroup vimrc_autocmds
    " autocmd BufEnter * highlight OverLength ctermbg=darkgrey guibg=#592929
    " autocmd BufEnter * match OverLength /\%74v.*/
" augroup END

set ignorecase
set smartcase

set mouse=a
set scrolloff=15

set vb

"" VIMRC Specifics
" Window movement keybinds
map <silent> <C-l> :wincmd l<CR>
map <silent> <C-h> :wincmd h<CR>
map <silent> <C-j> :wincmd j<CR>
map <silent> <C-k> :wincmd k<CR>

"" Here comes NERDTree specifics
" Close nerdtree if it's the only window left open in vim
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Keybindings
map <silent> <C-p> :NERDTreeToggle<CR>

" NERDTree symbols
let g:NERDTreeDirArrows = 1
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

syntax on
let base16colorspace=256
set background=dark
colorscheme base16-pop
" Enables transparent background
" hi Normal ctermbg=none
set colorcolumn=80
highlight ColorColumn ctermbg=DarkCyan
