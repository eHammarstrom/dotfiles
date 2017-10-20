if &compatible
  set nocompatible               " Be iMproved
endif

" Set python path
let g:python_host_prog='/usr/bin/python'

set runtimepath+=/home/initiumdoeslinux/.nvim/bundles/./repos/github.com/Shougo/dein.vim

if dein#load_state('/home/initiumdoeslinux/.nvim/bundles/.')
  call dein#begin('/home/initiumdoeslinux/.nvim/bundles/.')
  call dein#add('/home/initiumdoeslinux/.nvim/bundles/./repos/github.com/Shougo/dein.vim')

  call dein#add('Shougo/deoplete.nvim')
  call dein#add('scrooloose/nerdtree')
  call dein#add('vim-airline/vim-airline')
  call dein#add('jiangmiao/auto-pairs') " pairing ()
  call dein#add('justinmk/vim-syntax-extra') " better C syntax HL
  call dein#add('tpope/vim-surround') " S surround insert
  call dein#add('Shougo/vimproc.vim', {'build': 'make'})
  call dein#add('luochen1990/rainbow') " rainbow parens
  call dein#add('Shougo/echodoc.vim') " print func signatures
  call dein#add('w0rp/ale') " Linting
  call dein#add('autozimu/LanguageClient-neovim') " currently not very useful
  call dein#add('jreybert/vimagit') " magit
  call dein#add('airblade/vim-gitgutter') " annotate git diff by line number
  call dein#add('ctrlpvim/ctrlp.vim') " fuzzy fuzz
  call dein#add('editorconfig/editorconfig-vim') " conform with the norm
  call dein#add('dkasak/gruvbox') " better haskell / purescript support
  call dein#add('derekwyatt/vim-scala') " syntax
  call dein#add('godlygeek/tabular') " align stuff :Tabularize REGEX
  call dein#add('plasticboy/vim-markdown')
  call dein#add('ElmCast/elm-vim') " goodies for elm
  call dein#add('neovimhaskell/haskell-vim') " syntax & indent
  call dein#add('parsonsmatt/intero-neovim') " intero for vim
  call dein#add('eagletmt/neco-ghc') " haskell autocomplete
  call dein#add('mxw/vim-jsx') " jsx syntax
  call dein#add('pangloss/vim-javascript') " js syntax

  call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

  call dein#end()
  call dein#save_state()
endif

syntax enable
filetype plugin indent on

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

"End dein Scripts-------------------------
let mapleader = ","

" Tab handling
map <silent><Leader>q :tabp<CR>
map <silent><Leader>w :tabn<CR>

" Motions
nnoremap H ^
nnoremap L $
vnoremap H ^
vnoremap L g_

" Window switching
map <silent> <C-l> :wincmd l<CR>
map <silent> <C-h> :wincmd h<CR>
map <silent> <C-j> :wincmd j<CR>
map <silent> <C-k> :wincmd k<CR>

" Window resizing
map <silent> <C-left> :vertical res +2<CR>
map <silent> <C-right> :vertical res -2<CR>
map <silent> <C-up> :res -2<CR>
map <silent> <C-down> :res +2<CR>

" I never use these..
map <F4> :split term://zsh<CR>
map <F5> :vsplit term://zsh<CR>
tnoremap <Esc> <C-\><C-n>

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
colorscheme gruvbox
let g:gruvbox_contrast='soft'

""""""""""" here follows plugin specific settings
call deoplete#enable()

" NERD TREE
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <F2> :NERDTreeToggle<CR>
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
