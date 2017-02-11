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
    call dein#add('sebastianmarkow/deoplete-rust')
    call dein#add('vim-syntastic/syntastic')
    call dein#add('rust-lang/rust.vim')
    call dein#add('mattn/webapi-vim')

    " You can specify revision/branch/tag.
    call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

    " Required:
    call dein#end()
    call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

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

" YCM arch fixes
let g:ycm_path_to_python_interpreter = '/usr/bin/python2'
let g:ycm_global_ycm_extra_conf = '/usr/share/vim/vimfiles/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'

" racer config
let g:deoplete#sources#rust#racer_binary = '/home/initiumdoeslinux/.cargo/bin/racer'
let g:deoplete#sources#rust#rust_source_path = '/home/initiumdoeslinux/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src'

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_rust_checkers = ['rustc']

