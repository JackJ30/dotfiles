vim.g.mapleader = " "

-- LAZY
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
   vim.fn.system({
		 "git",
		 "clone",
		 "--filter=blob:none",
		 "https://github.com/folke/lazy.nvim.git",
		 "--branch=stable",
		 lazypath,
   })
end
vim.opt.rtp:prepend(lazypath)

-- Packages
require("lazy").setup({
   {
      "https://github.com/junegunn/fzf.vim",
      dependencies = {
	 "https://github.com/junegunn/fzf",
      },
      keys = {
	 { "<Leader><Leader>", "<Cmd>Files<CR>", desc = "Find files" },
	 { "<Leader>b", "<Cmd>Buffers<CR>", desc = "Find buffers" },
	 { "<Leader>/", "<Cmd>Rg<CR>", desc = "Search project" },
      },
   },
   {
      "https://github.com/stevearc/oil.nvim",
      config = function()
	 require("oil").setup()
      end,
      keys = {
	 { "-", "<Cmd>Oil<CR>", desc = "Browse files from here" },
      },
   },
   {
      "https://github.com/windwp/nvim-autopairs",
      event = "InsertEnter", -- Only load when you enter Insert mode
      config = function()
	 require("nvim-autopairs").setup()
      end,
   },
   {
      "https://github.com/numToStr/Comment.nvim",
      event = "VeryLazy", -- Special lazy.nvim event for things that can load later and are not important for the initial UI
      config = function()
	 require("Comment").setup()
      end,
   },
   {
      "https://github.com/tpope/vim-sleuth",
      event = { "BufReadPost", "BufNewFile" }, -- Load after your file content
   },
   {
      "nvim-treesitter/nvim-treesitter",
      build = ":TSUpdate",
      config = function () 
	 local configs = require("nvim-treesitter.configs")

	 configs.setup({
	    ensure_installed = { "c", "lua", "html", "zig" },
	    sync_install = false,
	    highlight = { enable = true },
	    indent = { enable = true },
	    incremental_selection = {
	       enable = true,
	       keymaps = {
		  node_incremental = "v",
		  node_decremental = "V",
	       },
	    },
	 })
      end
   },
   {
      "https://github.com/farmergreg/vim-lastplace",
      event = "BufReadPost",
   },
   {
      "https://github.com/NeogitOrg/neogit",
      dependencies = {
	 "nvim-lua/plenary.nvim",
	 "sindrets/diffview.nvim"
      },
      cmd = "Neogit", -- Only load when you run the Neogit command
      config = function()
	 require("neogit").setup()
      end,
   },
   {
      "https://github.com/Mofiqul/dracula.nvim",
      lazy=false,
      priority=1000,
   },
   {
      "neovim/nvim-lspconfig",
      dependencies = {
	 "williamboman/mason.nvim",
	 "williamboman/mason-lspconfig.nvim",
	 "L3MON4D3/LuaSnip",
	 'saghen/blink.cmp',
      },
      config = function()
	 local blink = require('blink.cmp')
	 local capabilities = vim.tbl_deep_extend(
	    "force",
	    {},
	    vim.lsp.protocol.make_client_capabilities(),
	    blink.get_lsp_capabilities())
	 require("mason").setup({
	    PATH = "append",
	 })
	 require("mason-lspconfig").setup({
	    ensure_installed = {
	       "lua_ls",
	       "zls",
	       "texlab",
	       "clangd",
	    },
	    handlers = {
	       function(server_name) -- default handler (optional)
		  require("lspconfig")[server_name].setup {
		     capabilities = capabilities
		  }
	       end,
	       ["zls"] = function ()
		  vim.g.zig_fmt_parse_errors = 0
		  vim.g.zig_fmt_autosave = 0
		  local lspconfig = require("lspconfig")
		  lspconfig.zls.setup({
		     capabilities = capabilities,
		     autoformat = false,
		  })
	       end,
	       ["lua_ls"] = function()
		  local lspconfig = require("lspconfig")
		  lspconfig.lua_ls.setup {
		     capabilities = capabilities,
		     settings = {
			Lua = {
			   runtime = { version = "Lua 5.1" },
			   diagnostics = {
			      globals = { "bit", "vim", "it", "describe", "before_each", "after_each" },
			   }
			}
		     }
		  }
	       end,
	    }
	 })
	 vim.diagnostic.config({
	    -- update_in_insert = true,
	    float = {
	       focusable = false,
	       style = "minimal",
	       border = "rounded",
	       source = true,
	       header = "",
	       prefix = "",
	    },
	 })
	 vim.api.nvim_create_autocmd('LspAttach', {
	    callback = function(event)
	       local opts = {buffer = event.buf}
	       vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
	       vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
	       vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
	       vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
	       vim.keymap.set('n', 'go', vim.lsp.buf.type_definition, opts)
	       vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
	       vim.keymap.set('n', 'gs', vim.lsp.buf.signature_help, opts)
	       vim.keymap.set('n', '<F2>', vim.lsp.buf.rename, opts)
	       vim.keymap.set({'n', 'x'}, '<F3>', vim.lsp.buf.format, opts)
	       vim.keymap.set('n', '<F4>', vim.lsp.buf.code_action, opts)
	    end,
	 })
      end
   },
   {
      'dnlhc/glance.nvim',
      cmd = 'Glance'
   },
   {
      'saghen/blink.cmp',
      version = "*",
      -- build = "cargo build --release",
      opts = {
	 snippets = { preset = 'luasnip', },
	 completion = {
	    keyword = {
	       range = 'prefix',
	    },
	    trigger = {
	       -- show_in_snippet = false,
	       -- show_on_keyword = false,
	       -- show_on_trigger_character = false,
	       -- show_on_accept_on_trigger_character = false,
	       -- show_on_insert_on_trigger_character = false,
	    },
	    list = {
	       selection = {
		  preselect = false,
		  auto_insert = false,
	       },
	    },
	    menu = {
	       draw = {
		  columns = { { "label", "label_description", gap = 1 }, { "kind" , "source_name", gap = 1} },
	       },
	    },
	    documentation = {
	       auto_show = true,
	    },
	    ghost_text = {
	       enabled = true,
	    },
	 },
	 fuzzy = {
	    prebuilt_binaries = {
	       download = true,
	    },
	 },
	 keymap = {
	    preset = 'default',
	    ['<Tab>'] = { 'accept', 'fallback' },
	    ['<CR>'] = { 'accept', 'fallback' },
	    ['<C-M-i>'] = { 'show', 'select_and_accept', 'fallback' },
	 },
	 cmdline = {
	    completion = {
	       list = {
		  selection = {
		     preselect = false,
		     auto_insert = false,
		  },
	       },
	    },
	    keymap = {
	       preset = 'cmdline',
	       ['<Tab>'] = { 'show', 'accept', 'fallback' },
	       ['<CR>'] = { 'accept', 'fallback' },
	       ['<C-M-i>'] = { 'show', 'select_and_accept', 'fallback' },
	    },
	 },
	 sources = {
	    default = { 'lsp', 'path', 'snippets', 'buffer' },
	    providers = {
	       buffer = {
		  name = 'Buffer',
		  module = 'blink.cmp.sources.buffer',
		  opts = {
		     -- all buffers of same filetype
		     get_bufnrs = function()
			return vim
			   .iter(vim.api.nvim_list_bufs())
			   :filter(
			      function (buf)
				 return vim.bo[buf].buftype ~= 'nofile'
				    and vim.bo[buf].filetype == vim.bo.filetype
			      end)
			   :totable()
		     end,
		  }
	       },
	    }
	 },
      },
   }
})

vim.cmd.colorscheme("dracula")

vim.opt.number = true
vim.opt.relativenumber = true

vim.opt.splitbelow = true
vim.opt.splitright = true

vim.o.shiftwidth = 4
vim.o.tabstop = 4;
vim.o.expandtab = false
vim.opt.signcolumn = "no"
vim.o.smartindent = true;

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.cache/nvim/undodir"
vim.opt.undofile = true

vim.opt.pumheight = 8

vim.opt.scrolloff = 7

vim.opt.list = true
vim.opt.listchars = { tab = "  " }

-- Map movement keys in insert and command modes
local map = vim.api.nvim_set_keymap
local opts = { noremap = true }

-- Insert mode mappings
map('i', '<C-h>', '<Left>', opts)
map('i', '<C-j>', '<Down>', opts)
map('i', '<C-k>', '<Up>', opts)
map('i', '<C-l>', '<Right>', opts)

-- Command-line mode mappings
map('c', '<C-h>', '<Left>', opts)
map('c', '<C-j>', '<Down>', opts)
map('c', '<C-k>', '<Up>', opts)
map('c', '<C-l>', '<Right>', opts)

map('i', '<C-g>', '<Esc>', opts)
map('v', '<C-g>', '<Esc>', opts)

vim.keymap.set('n', 'gR', '<CMD>Glance references<CR>')
