-- tabs
vim.opt.tabstop = 4
vim.opt.shiftwidth = 0
vim.opt.expandtab = false
-- display cursor at front of tabs
vim.opt.list = true
vim.opt.listchars = { tab = "  " }

vim.opt.shortmess:append { s = true, I = true }

vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'

-- line numbers
vim.opt.signcolumn="number"
vim.opt.number=true
vim.opt.fillchars:append({ eob = " " })
vim.opt.scrolloff = 7

vim.opt.splitright = true

vim.opt.cursorline = true

vim.diagnostic.config({
	virtual_text = true,
})

-- undo
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.cache/nvim/undodir"
vim.opt.undofile = true
