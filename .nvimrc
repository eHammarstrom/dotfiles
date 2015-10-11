" NVimRC iniShip

filetype off

call plug#begin('~/.vim/plugged')

" Theme
Plug 'chriskempson/base16-vim'

" Utilities
" Plug 'powerline/powerline'
" Plug 'bling/vim-airline'
" Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'ervandew/supertab'

" Language specifics
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'tpope/vim-rails'

call plug#end()

filetype plugin indent on

set shiftwidth=2
set softtabstop=2
set number
set ignorecase
set smartcase
set scrolloff=10
set vb

"" NVIMRC Specifics
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

syntax enable
let base16colorspace=256
set background=dark
colorscheme base16-atelierseaside
" Command below enables transparent backgrounds, which I didn't like.
" hi Normal ctermbg=none
