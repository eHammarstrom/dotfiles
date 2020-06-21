if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'bronson/vim-trailing-whitespace'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'w0rp/ale'
Plug 'luochen1990/rainbow'
Plug 'itchyny/lightline.vim'

" LLVM IR syntax hl
Plug 'rhysd/vim-llvm'

Plug 'sheerun/vim-polyglot'

" rust
Plug 'rust-lang/rust.vim'

" golang
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

" Themes
Plug 'chriskempson/base16-vim'
Plug 'rakr/vim-two-firewatch'
Plug 'morhetz/gruvbox'
Plug 'joshdick/onedark.vim'

call plug#end()

syntax enable
filetype plugin indent on

"""""" bindings

let mapleader = ','

" editing
nmap <BS> O<down><Esc>
nmap <CR> o<Esc>

" motions
nnoremap H ^
nnoremap E $
vnoremap H ^
vnoremap E g_

" window switching
map <silent><C-s> :wincmd l<CR>
map <silent><C-h> :wincmd h<CR>
map <silent><C-t> :wincmd j<CR>
map <silent><C-n> :wincmd k<CR>

" page up/down
" C-f, C-b for full page jump
noremap <C-u> <C-u>
noremap <C-e> <C-d>

" tag mania
noremap <leader>t <C-]>
set tags=tags;/

" Dvorak remap (https://gist.github.com/agnoster/640210)
" 1 - Movement keys htns -> hjkl
"   (gj and gk move by visual lines, if the line is wrapped for instance)
noremap h h
noremap t gj
noremap n gk
noremap s l
" 2 - replace functions we remapped away
" S goes to bottom of Screen
noremap S L
" j/J Jerk and replace a character/line
noremap j s
noremap J S
" k/K Keep searching through regex matches
noremap k n
noremap K N
" l/L Looks forward/backward to a
noremap l t
noremap L T
" T  puts lines Together
noremap T J

" window resizing
noremap <silent><left> :vertical res +1<CR>
noremap <silent><right> :vertical res -1<CR>
noremap <silent><up> :res -1<CR>
noremap <silent><down> :res +1<CR>

noremap <C-a> :Tabularize<space>/

"""""" settings

" use system clipboard
set clipboard=unnamedplus

" use relative above & below cursor
set relativenumber
" show absolute at cursor
set number

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
set completeopt=menu,menuone,noselect,noinsert
set tags=./tags,tags,../tags
" set hidden
set nobackup
set nowritebackup
set cmdheight=3
set updatetime=300
set shortmess+=c
set signcolumn=yes

" global tabstuff, personal preference
set autoindent expandtab tabstop=4 shiftwidth=4 colorcolumn=80
au FileType tex setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
au FileType latex setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
au FileType plaintex setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80

au FileType python setlocal autoindent expandtab tabstop=4 shiftwidth=4

" All .h files are for C, not C++
augroup C
    au!
    au BufRead,BufNewFile *.h,*.c set filetype=c
    au BufRead,BufNewFile *.overlay set filetype=dts syntax=dts
    " linux c kernel style
    au FileType c setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
    au FileType dts setlocal autoindent noexpandtab tabstop=4 shiftwidth=4 colorcolumn=80
    au FileType s,asm setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
augroup END

" THEME
set termguicolors
set t_Co=256
set background=dark
colorscheme onedark
highlight ColorColumn ctermbg=0 guibg=black

"""""" plugin settings

" fzf
let g:fzf_nvim_statusline = 0
let $FZF_DEFAULT_COMMAND = 'rg --files'

nnoremap <silent> <C-f> :Files<CR>
nnoremap <silent> <C-p> :Buffers<CR>

" rainbow parens
let g:rainbow_active = 1

" ALE

let g:ale_completion_max_suggestions = 100
let g:ale_completion_enabled = 1
let g:ale_fix_on_save = 0

let g:ale_linters = {
\   'rust': ['rls'],
\   'c': ['cquery'],
\   'python': ['pyls'],
\}

let g:ale_fixers = {
\   '*': ['trim_whitespace'],
\   'c': ['clang-format'],
\   'rust': ['rustfmt'],
\}

" lightline
set laststatus=2
