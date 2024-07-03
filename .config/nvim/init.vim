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
" :SudaWrite to write with sudo
Plug 'lambdalisue/suda.vim'

" LSP start
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-buffer'
Plug 'L3MON4D3/LuaSnip'
Plug 'saadparwaiz1/cmp_luasnip'
" LSP end

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

local lspconfig = require('lspconfig')
lspconfig.clangd.setup {}
-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Only bind autosave on write if the LSP is loaded
    vim.api.nvim_create_autocmd("BufWritePre", {
        buffer = buffer,
        callback = function()
            vim.lsp.buf.format { async = false }
        end
    })
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', '<leader>T', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', '<leader>t', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set("n","<leader>re", vim.lsp.buf.rename, bufopts)
    vim.keymap.set("n","<leader>ca", vim.lsp.buf.code_action, bufopts)
    -- vim.keymap.set('n', '<leader>t', vim.lsp.buf.implementation, opts)
    -- vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    -- vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    -- vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
    -- vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
    -- vim.keymap.set('n', '<space>wl', function()
    --   print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    -- end, opts)
    -- vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
    -- vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    -- vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
    -- vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', '<leader>ff', function()
        vim.lsp.buf.format { async = true }
    end, opts)
  end,
})

local cmp = require('cmp')

cmp.setup({
    -- Enable LSP completion
    sources = {
        { name = 'nvim_lsp' },
        { name = 'path' },
        { name = 'buffer' },
        { name = 'luasnip' },
    },
    completion = {
        completeopt = 'menu,menuone,noselect',
    },
    mapping = {
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<CR>'] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Insert,
            select = true,
        }),
    },
    snippet = {
        expand = function(args)
            require('luasnip').lsp_expand(args.body)
        end,
    },
})

cmp.setup.buffer({
    sources = {
        { name = 'nvim_lsp' },
        { name = 'path' },
        { name = 'buffer' },
    },
})

local keymap = {
    ['<Tab>'] = cmp.mapping.complete(),
}

cmp.setup({
    snippet = {
        expand = function(args)
            require('luasnip').lsp_expand(args.body)
        end,
    },
    mapping = {
        ['<C-CR>'] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Replace, select = true }),
        ['<C-n>'] = function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            else
                fallback()
            end
        end,
        ['<C-p>'] = function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            else
                fallback()
            end
        end,
    },
})

EOF
