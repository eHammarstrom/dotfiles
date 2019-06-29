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
Plug 'rakr/vim-two-firewatch'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

call plug#end()

syntax enable
filetype plugin indent on

"""""" bindings

let mapleader = ','

map <silent><leader>t :terminal<CR>

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

" global tabstuff, personal preference
set autoindent expandtab tabstop=4 shiftwidth=4 colorcolumn=80

" linux c kernel style
au FileType c,h setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80

" THEME
set termguicolors
set t_Co=256
set background=light
let g:two_firewatch_italics=1
colorscheme two-firewatch

"""""" plugin settings

" fzf
let g:fzf_nvim_statusline = 0
let $FZF_DEFAULT_COMMAND = 'rg --files'

nnoremap <silent> <C-f> :Files<CR>
nnoremap <silent> <C-p> :Buffers<CR>

" coc.nvim config
" trigger completion
inoremap <silent><expr> <c-space> coc#refresh()
" confirm completion
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" show docs
nnoremap <silent> K :call <SID>show_documentation()<CR>

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')
" use `:OrganizeImport` for organize import of current buffer
command! -nargs=0 OrganizeImport :call CocAction('runCommand', 'editor.action.organizeImport')

" highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

autocmd BufNewFile,BufRead *.rs CocEnable

set hidden
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
