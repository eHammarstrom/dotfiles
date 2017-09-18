"dein Scripts-----------------------------
if &compatible
    set nocompatible               " Be iMproved
endif

" Set python path
let g:python_host_prog='/usr/bin/python'

" Required:
set runtimepath+=/home/initiumdoeslinux/.nvim/bundles/./repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/home/initiumdoeslinux/.nvim/bundles/.')
    call dein#begin('/home/initiumdoeslinux/.nvim/bundles/.')

    " Let dein manage dein
    " Required:
    call dein#add('/home/initiumdoeslinux/.nvim/bundles/./repos/github.com/Shougo/dein.vim')

    " Add or remove your plugins here:
    call dein#add('neomake/neomake')
    call dein#add('Shougo/deoplete.nvim')
    call dein#add('eagletmt/neco-ghc')
    call dein#add('scrooloose/nerdtree')
    call dein#add('ervandew/supertab')
    call dein#add('jiangmiao/auto-pairs')
    call dein#add('justinmk/vim-syntax-extra')
    call dein#add('tpope/vim-surround')
    call dein#add('vim-syntastic/syntastic')
    call dein#add('mattn/webapi-vim')
    call dein#add('davidhalter/jedi-vim')
    call dein#add('Shougo/vimproc.vim', {'build': 'make'})

		" powerline
		call dein#add('vim-airline/vim-airline')

		" rainbow parens
		call dein#add('luochen1990/rainbow')

		" Git - vim magit
		call dein#add('jreybert/vimagit')
		call dein#add('airblade/vim-gitgutter') " show diff lines in vim

		" Projectile for VIM
		call dein#add('ctrlpvim/ctrlp.vim')

    " EditorConfig
    call dein#add('editorconfig/editorconfig-vim')

		" Folding
		call dein#add('pseewald/vim-anyfold')

    " call dein#add('morhetz/gruvbox')
    " call dein#add('chriskempson/base16-vim')
    call dein#add('dkasak/gruvbox') " better haskell / purescript support

		" Scala
		call dein#add('derekwyatt/vim-scala')
		call dein#add('ensime/ensime-vim')

		" Markdown
		call dein#add('godlygeek/tabular')
    call dein#add('plasticboy/vim-markdown')

    " Elm
    call dein#add('ElmCast/elm-vim')

    " Elixir
    call dein#add('elixir-lang/vim-elixir')
    call dein#add('slashmili/alchemist.vim')
    
    " Coconut
    call dein#add('manicmaniac/coconut.vim')
    
    " Haskell
    call dein#add('neovimhaskell/haskell-vim')
    " call dein#add('itchyny/vim-haskell-indent')
    call dein#add('alx741/vim-hindent')
    " call dein#add('eagletmt/ghcmod-vim')
    call dein#add('parsonsmatt/intero-neovim')

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

" Use system clipboard (X Window clipboard)
set clipboard=unnamedplus

set relativenumber
set nowrap
set smartcase
set incsearch
set mouse=a
set history=10000
set scrolloff=10

" global tabstuff, personal preference
set autoindent
set noexpandtab
set tabstop=2
set shiftwidth=2
set colorcolumn=120

" Folding (Plugin for better fold)
let anyfold_activate=1
let anyfold_fold_comments=1
set foldlevel=99
" disable markdown shitfold
let g:vim_markdown_folding_disabled = 1

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
let g:syntastic_javascript_checkers = ['eslint']
" let g:syntastic_rust_checkers = ['rustc'] " whines about unstable libraries
" even though they're stable

let g:syntastic_c_compiler_options = '-D_FILE_OFFSET_BITS=64'

let g:syntastic_mode_map = { 'passive_filetypes': ['scala'] }

" haskell
let g:haskell_classic_highlighting = 1
let g:haskell_enable_arrowsyntax = 1
let g:haskell_enable_typeroles = 1
let g:haskell_enable_pattern_synonyms = 1
let g:haskell_indent_disable = 1 " in favor of other flavor

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

" elm
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1

let g:elm_syntastic_show_warnings = 1

let g:haskell_indent_disable = 1 " disables vim-haskell indent in favor of hindent
let g:hindent_on_save = 1
let g:hindent_indent_size = 2
let g:hindent_line_length = 80

" javascript
let g:jsx_ext_required = 0

" scala ensime
au FileType scala nnoremap <silent> <leader>d :EnDeclaration<CR>
au FileType scala nnoremap <silent> <leader>r :EnRename<CR>
au FileType scala nnoremap <silent> <leader>t :EnType<CR>
au FileType scala nnoremap <silent> <leader>i :EnDocBrowse<CR>
autocmd BufWritePost *.scala silent :EnTypeCheck
" rainbow parens
let g:rainbow_active = 1

" powerline
let g:airline#extensions#tabline#enabled = 1
