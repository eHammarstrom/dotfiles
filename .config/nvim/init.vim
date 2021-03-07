if empty(glob('$HOME/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo '$HOME/.local/share/nvim/site/autoload/plug.vim' --create-dirs
              \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'joshdick/onedark.vim'
Plug 'luochen1990/rainbow'

Plug 'bronson/vim-trailing-whitespace'
Plug 'tpope/vim-surround'
Plug 'itchyny/lightline.vim'

Plug 'godlygeek/tabular'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

" LLVM IR syntax hl
Plug 'rhysd/vim-llvm'

Plug 'sheerun/vim-polyglot'

" rust
Plug 'rust-lang/rust.vim'

" golang
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

call plug#end()

syntax on
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

set list
set listchars=tab:>\ 

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
    au BufWritePost,BufRead,BufNewFile *.h,*.c set filetype=c
    au BufRead,BufNewFile *.overlay set filetype=dts syntax=dts
    " linux c kernel style
    au FileType c setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
    au FileType dts setlocal autoindent noexpandtab tabstop=4 shiftwidth=4 colorcolumn=80
    au FileType s,asm setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
    au FileType s,asm setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
augroup END

au FileType cpp setlocal autoindent noexpandtab tabstop=4 shiftwidth=4 colorcolumn=80
au FileType cmake setlocal autoindent noexpandtab tabstop=2 shiftwidth=2

" rust stuff
augroup RUST
    au!
    au FileType rust nnoremap <buffer> <silent><leader>t :ALEGoToDefinition<CR>
augroup END

" haskell
au FileType haskell,ocaml setlocal autoindent expandtab tabstop=2 shiftwidth=2 colorcolumn=110

" alang
au BufWritePost,BufRead,BufNewFile *.al set filetype=txt

" THEME
set termguicolors
set background=dark
colorscheme onedark
hi! ColorColumn ctermbg=NONE guibg=darkgrey
"""""" plugin settings

" fzf
let g:fzf_nvim_statusline = 0
let $FZF_DEFAULT_COMMAND = 'rg --files'

nnoremap <silent> <C-f> :Files<CR>
nnoremap <silent> <C-p> :Buffers<CR>

" rainbow parens
let g:rainbow_active = 1

" lightline
set laststatus=2

xnoremap <silent> <C-@> :w !wl-copy<CR><CR>
