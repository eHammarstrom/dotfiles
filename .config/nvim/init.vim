call plug#begin('~/.config/nvim/plugged')

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'tpope/vim-dispatch'
Plug 'sbdchd/neoformat'
Plug 'bronson/vim-trailing-whitespace'
Plug 'scrooloose/nerdtree'
Plug 'itchyny/lightline.vim'
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

" Haskell
Plug 'neovimhaskell/haskell-vim', { 'for': 'haskell' }

" JS
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'maxmellon/vim-jsx-pretty', { 'for': 'javascript' }

" Scala
Plug 'derekwyatt/vim-scala'

" Color
Plug 'pbrisbin/vim-syntax-shakespeare' " Haskell template syntax
Plug 'justinmk/vim-syntax-extra' " better C syntax HL
Plug 'luochen1990/rainbow' " rainbow parens
Plug 'rakr/vim-one'
Plug 'dkasak/gruvbox' " better haskell / purescript support
Plug 'nightsense/stellarized'
Plug 'yuttie/inkstained-vim' " inkstained
Plug 'NLKNguyen/papercolor-theme' " PaperColor

call plug#end()
call deoplete#enable()

syntax enable
filetype plugin indent on

let mapleader=","

set langmenu=en_US
let $LANG='en_US'

" Tab handling
map <silent><Leader> q :tabp<CR>
map <silent><Leader> w :tabn<CR>

" Editing
nmap <S-CR> O<Esc>
nmap <CR> o<Esc>

noremap <S-k> i<CR><Esc>

noremap <C-a> :Tabularize<space>/

noremap <silent><Leader> f :Neoformat<CR>

" Motions
nnoremap H ^
nnoremap E $
vnoremap H ^
vnoremap E g_

" Window switching
map <silent><C-s> :wincmd l<CR>
map <silent><C-h> :wincmd h<CR>
map <silent><C-t> :wincmd j<CR>
map <silent><C-n> :wincmd k<CR>

noremap <C-u> <C-b>
noremap <C-e> <C-f>

" Dvorak remap (https://gist.github.com/agnoster/640210)
" 1 - Movement keys htns -> hjkl
"   (gj and gk move by visual lines, if the line is wrapped for instance)
noremap h h
noremap t gj
noremap n gk
noremap s l
" 2 - replace functions we remapped away
"   S goes to bottom of Screen
noremap S L
"   j/J Jerk and replace a character/line
noremap j s
noremap J S
"   k/K Keep searching through regex matches
noremap k n
noremap K N
"   l/L Looks forward/backward to a
noremap l t
noremap L T
"   T   puts lines Together
noremap T J

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
set background=light
colorscheme PaperColor

let g:lightline = {
      \ 'colorscheme': 'PaperColor',
      \ 'component': {
      \   'readonly': '%{&readonly?"":""}',
      \ },
      \ 'separator':    { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '', 'right': '' },
      \ }

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


" javascript
let g:ale_fixers = {
\   'javascript': ['eslint'],
\}
let g:ale_fix_on_save = 1


let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1

