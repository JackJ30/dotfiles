vim.opt.tabstop = 4
vim.opt.shiftwidth = 0
vim.opt.expandtab = false

vim.opt.shortmess:append { s = true, I = true }

vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'

vim.opt.signcolumn="number"
vim.opt.number=true
vim.opt.fillchars:append({ eob = " " })

vim.diagnostic.config({
	virtual_text = true,
	update_in_insert = true,
})
