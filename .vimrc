" VimRC iniShip

filetype off

call plug#begin('~/.vim/plugged')

" Theme
Plug 'chriskempson/base16-vim'
Plug 'junegunn/goyo.vim'

" Utilities
Plug 'scrooloose/syntastic'
Plug 'scrooloose/nerdtree'
Plug 'jiangmiao/auto-pairs'

" Language specifics
Plug 'vim-ruby/vim-ruby'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'tpope/vim-rails'
Plug 'klen/python-mode'
Plug 'StanAngeloff/php.vim'
Plug 'maksimr/vim-jsbeautify'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'derekwyatt/vim-scala'

call plug#end()

filetype plugin indent on

" The boss of all productivity
let mapleader = "\<Space>"

set encoding=utf-8

" This enables powerline
set laststatus=2

set tabstop=2
set softtabstop=2
set shiftwidth=2
set noexpandtab

set number

" Enable folding
set foldmethod=indent
set foldlevel=99

" line wrapping
" set tw=80
" set fo=cqt
" set wm=0

" line wrapping test
" augroup vimrc_autocmds
" autocmd BufEnter * highlight OverLength ctermbg=darkgrey guibg=#592929
" autocmd BufEnter * match OverLength /\%74v.*/
" augroup END

set ignorecase
set smartcase

set mouse=a
set scrolloff=15

set vb

"" VIMRC Specifics
" Window movement keybinds
map <silent> <C-l> :wincmd l<CR>
map <silent> <C-h> :wincmd h<CR>
map <silent> <C-j> :wincmd j<CR>
map <silent> <C-k> :wincmd k<CR>

map <silent> <C-left> :vertical res +5<CR>
map <silent> <C-right> :vertical res -5<CR>
map <silent> <C-up> :res -5<CR>
map <silent> <C-down> :res +5<CR>

map <silent> <C-g> :Goyo <CR>

" Go to tab by number
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>0 :tablast<cr>

" Go to previous tab
nnoremap <leader>l :tabprevious<CR>

"" SCRUB Formatting
" autocmd FileType javascript noremap <buffer> <C-f> :call JsBeautify()<CR>
" for json
" autocmd FileType json noremap <buffer> <C-f> :call JsonBeautify()<CR>
" for jsx
" autocmd FileType jsx noremap <buffer> <C-f> :call JsxBeautify()<CR>
" for html
" autocmd FileType html noremap <buffer> <C-f> :call HtmlBeautify()<CR>
" for css or scss
" autocmd FileType css noremap <buffer> <C-f> :call CSSBeautify()<CR>

"" Here comes NERDTree specifics
" Close nerdtree if it's the only window left open in vim
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Keybindings
map <silent> <C-p> :NERDTreeToggle<CR>

" NERDTree symbols
let g:NERDTreeDirArrows = 1
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

syntax on
set background=dark
let base16colorspace=256
colorscheme base16-solarized
" Enables transparent background
" hi Normal ctermbg=none
set colorcolumn=80
highlight ColorColumn ctermbg=DarkCyan

let g:airline_powerline_fonts = 1
let g:airline_theme= 'light'

" Put at the very end of your .vimrc file.
function! PhpSyntaxOverride()
	hi! def link phpDocTags  phpDefine
	hi! def link phpDocParam phpType
endfunction

augroup phpSyntaxOverride
	autocmd!
	autocmd FileType php call PhpSyntaxOverride()
augroup END
