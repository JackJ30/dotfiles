vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- vim.keymap.set("i", "<C-BS>", "<C-w>")
-- vim.keymap.set("v", "<", "<gv")
-- vim.keymap.set("v", ">", ">gv")

-- vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
-- vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

-- vim.keymap.set("n", "<leader>k", "<cmd>:bd<CR>")

vim.keymap.set("n", "<leader>=", FormatBuffer)

-- vim.keymap.set({"n", "v"}, "<leader>d", "\"_d")
vim.keymap.set("n", "<leader>p", "\"+p")
vim.keymap.set("v", "<leader>y", "\"+y")

-- vim.keymap.set({"n", "i", "v", "s"}, "<F1>", "<Nop>")

vim.keymap.set("n", "<leader>=", FormatBuffer)

-- insert mode moving
vim.keymap.set('i', '<C-h>', '<Left>', opts)
vim.keymap.set('i', '<C-j>', '<Down>', opts)
vim.keymap.set('i', '<C-k>', '<Up>', opts)
vim.keymap.set('i', '<C-l>', '<Right>', opts)

-- C-g to escape
vim.keymap.set('i', '<C-g>', '<Esc>', opts)
vim.keymap.set('v', '<C-g>', '<Esc>', opts)
