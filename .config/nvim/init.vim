call plug#begin('~/.config/nvim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'neomake/neomake'
Plug 'tpope/vim-dispatch'
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
Plug 'autozimu/LanguageClient-neovim', {
      \ 'branch': 'next',
      \ 'do': './install.sh'
      \ }
Plug 'w0rp/ale'
Plug 'vim-syntastic/syntastic', { 'for': 'cs' }

" Haskell
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }

" JS
Plug 'moll/vim-node', { 'for': 'javascript' }
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'othree/yajs.vim', { 'for': 'javascript' }

" Scala
Plug 'derekwyatt/vim-scala'


" C#
Plug 'OmniSharp/omnisharp-vim'
Plug 'OrangeT/vim-csharp'

" Color
Plug 'pbrisbin/vim-syntax-shakespeare' " Haskell template syntax
Plug 'justinmk/vim-syntax-extra' " better C syntax HL
Plug 'luochen1990/rainbow' " rainbow parens
Plug 'rakr/vim-one'
Plug 'dkasak/gruvbox' " better haskell / purescript support

call plug#end()
call deoplete#enable()

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
au FileType haskell nnoremap <leader>lf :call LanguageClient_textDocument_formatting()<CR>

let g:LanguageClient_autoStart = 1
let g:LanguageClient_serverCommands = {
      \ 'haskell': ['hie', '--lsp'],
      \ }

let g:jsx_ext_required = 0 " enable jsx syntax for js files
autocmd BufWritePre *.js Neoformat

" Deoplete
" let g:deoplete#sources.cs = ['omni', 'file', 'buffer', 'ultisnips']
" let g:deoplete#omni#input_patterns.cs = ['\w*']
" let g:deoplete#enable_smart_case = 1

" C#
let g:OmniSharp_server_type = 'roslyn'
" let g:OmniSharp_server_path = '/Users/ehammarstrom/DevApps/omnisharp.http-osx'
autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck
autocmd FileType cs setlocal omnifunc=OmniSharp#Complete

" ALE
let g:ale_pattern_options = {
      \   '.*\.cs$': {'ale_enabled': 0},
      \}

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_cs_checkers = ['code_checker']
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1

