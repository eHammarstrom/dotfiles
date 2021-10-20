"""""" download plug.vim if not in path
if empty(glob($HOME . "/.local/share/nvim/site/autoload/plug.vim"))
	let cmd = 'curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

	execute "!" . cmd
endif


"""""" plugins
call plug#begin('~/.local/share/nvim/plugged')

Plug 'bronson/vim-trailing-whitespace'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'w0rp/ale'
Plug 'luochen1990/rainbow'

" Jenkins syntax
Plug 'martinda/Jenkinsfile-vim-syntax'

" LLVM IR syntax hl
Plug 'rhysd/vim-llvm'

Plug 'sheerun/vim-polyglot'

" golang
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

" Themes
Plug 'joshdick/onedark.vim'

" Plug 'Yggdroot/indentLine'

call plug#end()

syntax enable
filetype plugin indent on

"""""" bindings

let mapleader = ','

" editing
nmap <BS> O<down><Esc>
nmap <CR> o<Esc>
" goto tag
noremap <leader>t <C-]>

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

" Toggle keybind between expand noexpand of tabs
set expandtab
function TabToggle()
  if &expandtab
    set noexpandtab
  else
    set expandtab
  endif
endfunction
nmap <leader>e mz:execute TabToggle()<CR>'z

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
set list

" global tabstuff, personal preference
set autoindent expandtab tabstop=4 shiftwidth=4 colorcolumn=80
au FileType tex setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
au FileType latex setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
au FileType plaintex setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80

au FileType python setlocal autoindent expandtab tabstop=4 shiftwidth=4

au FileType cmake setlocal autoindent expandtab tabstop=2 shiftwidth=2

au FileType javascript setlocal autoindent noexpandtab tabstop=2 shiftwidth=2

augroup project
  autocmd!
  autocmd BufRead,BufNewFile *.h,*.c set filetype=c
augroup END

" kernel c style
au FileType cmake,c,h,header setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80,81,82
au FileType cpp setlocal autoindent noexpandtab tabstop=4 shiftwidth=4 colorcolumn=80,81,82
" device tree style
au FileType dts setlocal autoindent noexpandtab tabstop=8 shiftwidth=8

"autocmd BufRead,BufNewFile *.h,*.c setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
" asm
au FileType s,asm setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80

au FileType rust noremap <buffer><silent><leader>t :ALEGoToDefinition<CR>

" THEME
set termguicolors
set t_Co=256
set background=dark
colorscheme onedark
highlight ColorColumn guibg=Black ctermbg=7
" let g:two_firewatch_italics=1
" colorscheme two-firewatch
" colorscheme base16-summerfruit-dark
" colorscheme base16-irblack

"""""" plugin settings

" fzf
let g:fzf_nvim_statusline = 0
let $FZF_DEFAULT_COMMAND = 'rg --files'

nnoremap <silent> <C-f> :Files<CR>
nnoremap <silent> <C-p> :Buffers<CR>

" indentLine
let g:indentLine_enabled = 1
let g:indentLine_setColors = 0

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
\   'ocaml': ['ols'],
\}

let g:ale_fixers = {
\   '*': ['trim_whitespace'],
\   'c': ['clang-format'],
\   'rust': ['rustfmt'],
\}

set tags=./tags,tags;$HOME

let g:ale_ocaml_ols_executable = "ocamllsp"
