"dein Scripts-----------------------------
if &compatible
    set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/home/initiumdoeslinux/.nvim/bundles/./repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/home/initiumdoeslinux/.nvim/bundles/.')
    call dein#begin('/home/initiumdoeslinux/.nvim/bundles/.')

    " Let dein manage dein
    " Required:
    call dein#add('/home/initiumdoeslinux/.nvim/bundles/./repos/github.com/Shougo/dein.vim')

    " Add or remove your plugins here:
    call dein#add('Shougo/deoplete.nvim')
    call dein#add('eagletmt/ghcmod-vim')
    call dein#add('eagletmt/neco-ghc')
    call dein#add('scrooloose/nerdtree')
    call dein#add('ervandew/supertab')
    call dein#add('junegunn/fzf', { 'build': './install', 'merged': 0 })
    call dein#add('plasticboy/vim-markdown')
    call dein#add('jiangmiao/auto-pairs')
    call dein#add('justinmk/vim-syntax-extra')
    call dein#add('tpope/vim-surround')
    call dein#add('vim-syntastic/syntastic')
    call dein#add('mattn/webapi-vim')
    call dein#add('davidhalter/jedi-vim')
    call dein#add('Shougo/vimproc.vim', {'build': 'make'})

    " call dein#add('morhetz/gruvbox')
    " call dein#add('chriskempson/base16-vim')
    call dein#add('dkasak/gruvbox') " better haskell / purescript support
    
    " Coconut
    call dein#add('manicmaniac/coconut.vim')
    
    " Haskell
    call dein#add('neovimhaskell/haskell-vim')

    " Rust
    call dein#add('sebastianmarkow/deoplete-rust')
    call dein#add('rust-lang/rust.vim')

    " JavaScript
    call dein#add('mxw/vim-jsx')
    call dein#add('pangloss/vim-javascript')

    " You can specify revision/branch/tag.
    call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

    " Required:
    call dein#end()
    call dein#save_state()
endif

" Required:
syntax on
filetype plugin indent on

" If you want to install not installed plugins on startup.
if dein#check_install()
    call dein#install()
endif

"End dein Scripts-------------------------
let mapleader = "\<Space>"

nnoremap H ^
nnoremap L $
vnoremap H ^
vnoremap L g_

map <silent> <C-l> :wincmd l<CR>
map <silent> <C-h> :wincmd h<CR>
map <silent> <C-j> :wincmd j<CR>
map <silent> <C-k> :wincmd k<CR>

map <silent> <C-left> :vertical res +2<CR>
map <silent> <C-right> :vertical res -2<CR>
map <silent> <C-up> :res -2<CR>
map <silent> <C-down> :res +2<CR>

map <F4> :split term://zsh<CR>
map <F5> :vsplit term://zsh<CR>
tnoremap <Esc> <C-\><C-n>

set relativenumber
set nowrap
set smartcase
set smarttab
set smartindent
set autoindent
set softtabstop=4
set shiftwidth=4
set expandtab
set incsearch
set mouse=a
set history=10000
set scrolloff=7

highlight Pmenu ctermbg=235 ctermfg=207 gui=bold

autocmd FileType c call CInitialize()

function CInitialize()
    setlocal softtabstop=8
    setlocal shiftwidth=8
    setlocal colorcolumn=80
    map <buffer> <leader>c :call CQuickCompile()<CR>
    map <buffer> <leader>e :execute '!./' . expand('%:r')<CR>
endfunction

function CQuickCompile()
    let curBufFileName = expand('%:t')
    let outName = expand('%:r')
    execute '!gcc ' . curBufFileName . ' -o ' . outName
endfunction

" THEME
set termguicolors
set t_Co=256
set background=dark
colorscheme gruvbox

""""""""""" here follows plugin specific settings
call deoplete#enable()

" NERD TREE
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <F2> :NERDTreeToggle<CR>
map <F3> :NERDTreeFind<CR>
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

" FZF
map <C-p> :FZF<CR>
map <C-n> :FZF ~<CR>

" Supertab reverse completion
let g:SuperTabDefaultCompletionType = '<C-n>'

" rust racer
let g:deoplete#sources#rust#racer_binary = '/home/initiumdoeslinux/.cargo/bin/racer'
let g:deoplete#sources#rust#rust_source_path = '/home/initiumdoeslinux/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'
let g:deoplete#sources#rust#documentation_max_height=20

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
" let g:syntastic_rust_checkers = ['rustc'] " whines about unstable libraries
" even though they're stable

let g:syntastic_c_compiler_options = '-D_FILE_OFFSET_BITS=64'

" haskell
let g:haskell_classic_highlighting = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1

" javascript
let g:jsx_ext_required = 0
