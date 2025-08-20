vim.api.nvim_set_option("clipboard", "unnamedplus")

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git", lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)
require("lazy").setup({
    { 'nvim-telescope/telescope.nvim', branch = '0.1.x', dependencies = { 'nvim-lua/plenary.nvim' } },
    { 'navarasu/onedark.nvim' },
    { 'hrsh7th/nvim-cmp' },
    { "github/copilot.vim" },
    { "junegunn/tabularize" },
    { "neovim/nvim-lspconfig" },
    { "tpope/vim-surround" },
})

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

vim.lsp.enable('clangd')
vim.lsp.config('clangd', {
    cmd = { "clangd", "--offset-encoding=utf-16" }
})
vim.lsp.enable('rust_analyzer')
-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'
    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', '<leader>T', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', '<leader>t', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set("n","<leader>re", vim.lsp.buf.rename, bufopts)
    vim.keymap.set("n","<leader>ca", vim.lsp.buf.code_action, bufopts)
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
