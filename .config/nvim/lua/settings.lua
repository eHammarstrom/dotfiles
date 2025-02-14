local rocks_config = {
	rocks_path = vim.env.HOME .. "/.local/share/nvim/rocks",
}

vim.g.rocks_nvim = rocks_config

local luarocks_path = {
	vim.fs.joinpath(rocks_config.rocks_path, "share", "lua", "5.1", "?.lua"),
	vim.fs.joinpath(rocks_config.rocks_path, "share", "lua", "5.1", "?", "init.lua"),
}
package.path = package.path .. ";" .. table.concat(luarocks_path, ";")

local luarocks_cpath = {
	vim.fs.joinpath(rocks_config.rocks_path, "lib", "lua", "5.1", "?.so"),
	vim.fs.joinpath(rocks_config.rocks_path, "lib64", "lua", "5.1", "?.so"),
}
package.cpath = package.cpath .. ";" .. table.concat(luarocks_cpath, ";")

vim.opt.runtimepath:append(vim.fs.joinpath(rocks_config.rocks_path, "lib", "luarocks", "rocks-5.1", "rocks.nvim", "*"))

---------- SETTINGS ----------

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
lspconfig.rust_analyzer.setup {}
-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Only bind autosave on write if the LSP is loaded
    -- vim.api.nvim_create_autocmd("BufWritePre", {
        -- buffer = buffer,
        -- callback = function()
            -- vim.lsp.buf.format { async = false }
        -- end
    -- })
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

require('fugit2').setup({
})
