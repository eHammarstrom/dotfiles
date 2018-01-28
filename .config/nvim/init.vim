call plug#begin('~/.config/nvim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'neomake/neomake'
Plug 'sbdchd/neoformat'
Plug 'bronson/vim-trailing-whitespace'
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'jreybert/vimagit'
Plug 'airblade/vim-gitgutter'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'editorconfig/editorconfig-vim' " conform to the norm
Plug 'godlygeek/tabular' " :Tabularize REGEX
Plug 'plasticboy/vim-markdown'
Plug 'sbdchd/neoformat'

" Haskell
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }
Plug 'Twinside/vim-hoogle', { 'for': 'haskell' }
Plug 'parsonsmatt/intero-neovim', { 'for': 'haskell' }

" JS
Plug 'moll/vim-node', { 'for': 'javascript' }
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'othree/yajs.vim', { 'for': 'javascript' }
Plug 'prettier/vim-prettier', {
      \ 'do': 'npm install',
      \ 'for': ['javascript', 'typescript', 'css', 'less', 'scss'] }

" Scala
Plug 'derekwyatt/vim-scala'

" Color
Plug 'pbrisbin/vim-syntax-shakespeare' " Haskell template syntax
Plug 'justinmk/vim-syntax-extra' " better C syntax HL
Plug 'luochen1990/rainbow' " rainbow parens
Plug 'rakr/vim-one'
Plug 'dkasak/gruvbox' " better haskell / purescript support

call plug#end()

syntax enable
filetype plugin indent on

let mapleader = ","

set langmenu=en_US
let $LANG = 'en_US'

" Tab handling
map <silent><Leader> q :tabp<CR>
map <silent><Leader> w :tabn<CR>

" Editing
nmap <S-CR> O<Esc>
nmap <CR> o<Esc>

noremap <S-k> i<CR><Esc>

" Motions
nnoremap H ^
nnoremap L $
vnoremap H ^
vnoremap L g_

" Window switching
map <silent><C-l> :wincmd l<CR>
map <silent><C-h> :wincmd h<CR>
map <silent><C-j> :wincmd j<CR>
map <silent><C-k> :wincmd k<CR>

" Window resizing
noremap <silent><left> :vertical res +1<CR>
noremap <silent><right> :vertical res -1<CR>
noremap <silent><up> :res -1<CR>
noremap <silent><down> :res +1<CR>

set clipboard=unnamedplus " use system clipboard

set relativenumber " use relative above & below cursor
set number " show absolute at cursor

" soft wrapping
set wrap
set linebreak
set nolist

set smartcase
set incsearch
set mouse=a
set history=10000
set scrolloff=10

set complete=.,w,b,u,U,d,k,t
set tags=./tags,tags,../tags

" global tabstuff, personal preference
set autoindent
set expandtab
set tabstop=2
set shiftwidth=2
set colorcolumn=120

" linux c kernel style
au FileType c setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80 

" THEME
set termguicolors
set t_Co=256
set background=dark
colorscheme one

""""""""""" here follows plugin specific settings
call deoplete#enable()

" NERD TREE
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <S-f> :NERDTreeToggle<CR>
map <F3> :NERDTreeFind<CR>
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

" CtrlP
" law abiding citizen, .gitignore
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" disable markdown shitfold
let g:vim_markdown_folding_disabled = 1

" haskell
let g:haskell_classic_highlighting = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1

let g:haskellmode_completion_ghc = 0 " Disable haskell-vim omnifunc
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

augroup interoMaps
  au!
  " Maps for intero. Restrict to Haskell buffers so the bindings don't collide.

  " Open intero/GHCi split horizontally
  au FileType haskell nnoremap <silent> <leader>io :InteroOpen<CR>
  " Open intero/GHCi split vertically
  au FileType haskell nnoremap <silent> <leader>iov :InteroOpen<CR><C-W>H
  au FileType haskell nnoremap <silent> <leader>ih :InteroHide<CR>

  " Automatically reload on save
  au BufWritePost *.hs InteroReload

  " Load individual modules
  au FileType haskell nnoremap <silent> <leader>il :InteroLoadCurrentModule<CR>
  au FileType haskell nnoremap <silent> <leader>if :InteroLoadCurrentFile<CR>

  " Type-related information
  " Heads up! These next two differ from the rest.
  au FileType haskell map <silent> <leader>t <Plug>InteroGenericType
  au FileType haskell map <silent> <leader>T <Plug>InteroType
  au FileType haskell nnoremap <silent> <leader>it :InteroTypeInsert<CR>

  " Navigation
  au FileType haskell nnoremap <silent> <leader>jd :InteroGoToDef<CR>
augroup END

let g:jsx_ext_required = 0 " enable jsx syntax for js files
autocmd BufWritePre *.js Neoformat
