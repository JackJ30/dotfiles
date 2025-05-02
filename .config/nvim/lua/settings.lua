vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.smartindent = true
vim.opt.undodir = os.getenv("HOME") .. "/.cache/nvim/undodir"
vim.opt.undofile = true
vim.opt.swapfile = false

vim.opt.relativenumber = true
vim.opt.number = true

vim.opt.scrolloff = 7

vim.opt.termguicolors = true
vim.opt.background = "dark"

vim.opt.incsearch = true

vim.opt.signcolumn = "no"
vim.opt.isfname:append("@-@")

vim.opt.splitbelow = true
vim.opt.splitright = true

