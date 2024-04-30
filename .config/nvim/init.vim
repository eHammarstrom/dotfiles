""""" download plug.vim if not in path
if empty(glob($HOME . "/.local/share/nvim/site/autoload/plug.vim"))
	let cmd = 'curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

	execute "!" . cmd
endif

call plug#begin('~/.local/share/nvim/plugged')
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'github/copilot.vim'
Plug 'bronson/vim-trailing-whitespace'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'luochen1990/rainbow'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'joshdick/onedark.vim'
Plug 'Olical/conjure'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-neorg/neorg'
Plug 'neovim/nvim-lspconfig'
call plug#end()

syntax enable
filetype plugin indent on

"""""" bindings

let mapleader = ','
let maplocalleader = ','

" editing
nmap <BS> O<down><Esc>
nmap <CR> o<Esc>
" goto tag
noremap <leader>t <C-]>

" compile
map <silent><leader>zz :make<CR>

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
nmap <leader>ww mz:execute TabToggle()<CR>'z

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

set tags=./tags,tags;$HOME

au FileType cmake,c,h,header setlocal autoindent noexpandtab tabstop=8 shiftwidth=8
let g:toggleC = 0
function! GruggC()
    if g:toggleC == 0
        " grugg mode
        set expandtab
        set tabstop=2
        set shiftwidth=2
        let g:toggleC = 1
    else
        set noexpandtab
        set tabstop=8
        set shiftwidth=8
        let g:toggleC = 0
    endif
endfunction
nmap <leader>g mz:execute GruggC()<CR>'z

" global tabstuff, personal preference
set autoindent expandtab tabstop=4 shiftwidth=4 colorcolumn=120
au FileType tex setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
au FileType latex setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
au FileType plaintex setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
au FileType cmake setlocal autoindent expandtab tabstop=2 shiftwidth=2
au FileType javascript setlocal autoindent noexpandtab tabstop=2 shiftwidth=2

augroup project
  autocmd!
  autocmd BufRead,BufNewFile *.h,*.c set filetype=c
augroup END

" au FileType cpp setlocal autoindent noexpandtab tabstop=4 shiftwidth=4 colorcolumn=80,81,82
" device tree style
au FileType dts setlocal autoindent noexpandtab tabstop=8 shiftwidth=8
au FileType s,asm setlocal autoindent noexpandtab tabstop=8 shiftwidth=8 colorcolumn=80
au FileType rust noremap <buffer><silent><leader>t :ALEGoToDefinition<CR>
au FileType zig noremap <buffer><silent><leader>t :ALEGoToDefinition<CR>
au FileType zig noremap <buffer><silent><leader>zz :make<CR>

" THEME
set termguicolors
set t_Co=256
set background=dark
colorscheme onedark
highlight ColorColumn guibg=Black ctermbg=7
set cursorline

"""""" plugin settings

" rainbow parens
let g:rainbow_active = 1

lua << EOF

local autocmd = vim.api.nvim_create_autocmd
local builtin = require "telescope.builtin"
local actions = require "telescope.actions"
local action_set = require "telescope.actions.set"
local action_init = require "telescope.actions.init"
local action_state = require "telescope.actions.state"

require("telescope").setup {
    defaults = {
        file_ignore_patterns = { "target/", "sdk/" },
        prompt_prefix = "λ ",
        ripgrep_arguments = {
          'rg',
          '--hidden',
          '--ignore-vcs',
          '--no-heading',
          '--with-filename',
          '--line-number',
          '--column',
          '--smart-case'
        },
    },
    pickers = {
        find_files = {
            no_ignore = true,
        },
    }
}

vim.keymap.set("n", "<C-f>", builtin.find_files, {})
vim.keymap.set("n", "<C-p>", builtin.buffers, {})
vim.keymap.set("n", "<C-g>", builtin.live_grep, {})
vim.keymap.set("n", "<leader>ht", function()
        builtin.help_tags({
            attach_mappings = function(prompt_bufnr, map)
                map('i', '<cr>', function()
                    require('telescope.actions').select_tab()
                end)
                return true
            end,
        })
    end, {})
vim.keymap.set("n", "<leader>hm", function()
        builtin.man_pages({ sections = { "ALL" } })
    end, {})

vim.g["conjure#mapping#doc_word"] = false

require("nvim-treesitter.configs").setup {
    ensure_installed = "all",
    sync_install = false,
    auto_install = true,
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
    },
}

require("neorg").setup {
    load = {
        ["core.defaults"] = {}, -- Loads default behaviour
        ["core.concealer"] = {}, -- Adds pretty icons to your documents
        ["core.dirman"] = { -- Manages Neorg workspaces
            config = {
                workspaces = {
                    brain = "~/notes/brain",
                },
                default_workspace = "brain",
            },
        },
    },
}

autocmd("FileType", {
    pattern = { "norg" },
    command = "noremap <buffer><silent<leader>ncc :Neorg toggle-concealer<CR>",
})


function on_attach(client, bufnr)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo.omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = bufnr }
    vim.keymap.set('n', '<leader>T', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', '<leader>t', vim.lsp.buf.definition, opts)
    -- vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set("n","<leader>re", vim.lsp.buf.rename, bufopts)
    vim.keymap.set("n","<leader>ca", vim.lsp.buf.code_action, bufopts)
end

local servers = { 'clangd', 'pyright' }
for _, lsp in ipairs(servers) do
    require('lspconfig')[lsp].setup({
        on_attach = on_attach,
        flags = { debounce_text_changes = 150, }
})
end

EOF
