" VimRC iniShip

filetype off

call plug#begin('~/.vim/plugged')

" Theme
"Plug 'chriskempson/base16-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Utilities
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'jiangmiao/auto-pairs'

" Fatih tips n tricks
Plug 'fatih/vim-go'
Plug 'fatih/molokai'
Plug 'SirVer/ultisnips'
Plug 'Valloric/YouCompleteMe'

" Solid language pack
Plug 'sheerun/vim-polyglot'

" HTML
Plug 'mattn/emmet-vim'

call plug#end()

filetype plugin indent on

" The boss of all productivity
let mapleader = "\<Space>"

set encoding=utf-8

" This enables powerline
set laststatus=2

set tabstop=2
set softtabstop=2
set shiftwidth=2
set noexpandtab

set number

" Enable folding
set foldmethod=indent
set foldlevel=99

set ignorecase
set smartcase

set mouse=a
set scrolloff=15

set vb

"" Language specifics
let g:polyglot_disabled = ['go']
autocmd BufNewFile,BufRead *.go setlocal softtabstop=8 shiftwidth=8 tabstop=8 shiftwidth=8

autocmd FileType go nmap <leader>b  <Plug>(go-build)
autocmd FileType go nmap <leader>r  <Plug>(go-run)
autocmd FileType go nmap <leader>d  <Plug>(go-doc)

let g:go_auto_sameids = 1

setlocal omnifunc=go#complete#Complete

"" VIMRC Specifics
nnoremap H ^
nnoremap L $
vnoremap H ^
vnoremap L g_

" Window movement keybinds
map <silent> <C-l> :wincmd l<CR>
map <silent> <C-h> :wincmd h<CR>
map <silent> <C-j> :wincmd j<CR>
map <silent> <C-k> :wincmd k<CR>

map <silent> <C-left> :vertical res +5<CR>
map <silent> <C-right> :vertical res -5<CR>
map <silent> <C-up> :res -5<CR>
map <silent> <C-down> :res +5<CR>

" Go to tab by number
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>0 :tablast<CR>

" Go to previous tab
nnoremap <leader>l :tabprevious<CR>

" Here comes NERDTree specifics
" Close nerdtree if it's the only window left open in vim
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Keybindings
map <silent> <C-p> :NERDTreeToggle<CR>

" NERDTree symbols
let g:NERDTreeDirArrows = 1
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
let g:airline_powerline_fonts = 1
let g:airline_theme = 'light'

syntax on
set t_Co=256
"set background=dark
"let base16colorspace=256
"colorscheme base16-tomorrow-night
" FATIH Theme
let g:rehash256 = 1
let g:molokai_original = 1
colorscheme molokai

set colorcolumn=80
highlight ColorColumn ctermbg=DarkCyan
