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
	call dein#add('Shougo/neosnippet.vim')
	call dein#add('Shougo/neosnippet-snippets')
	call dein#add('Shougo/deoplete.nvim')
	call dein#add('eagletmt/ghcmod-vim')
	call dein#add('eagletmt/neco-ghc')
	call dein#add('scrooloose/nerdtree')
	call dein#add('ervandew/supertab')
        call dein#add('junegunn/fzf', { 'build': './install', 'merged': 0 })

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
let g:SuperTabDefaultCompletionType = "<C-n>"

set relativenumber
set nowrap
set tw=80
set smartcase
set smarttab
set smartindent
set autoindent
set softtabstop=2
set shiftwidth=2
set expandtab
set incsearch
set mouse=a
set history=10000
