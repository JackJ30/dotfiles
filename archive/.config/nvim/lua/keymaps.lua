-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Move to window using the <C-hjkl> keys
vim.keymap.set('n', '<C-h>', '<C-w>h', { desc = 'Switch to left window' })
vim.keymap.set('n', '<C-j>', '<C-w>j', { desc = 'Switch to lower window' })
vim.keymap.set('n', '<C-k>', '<C-w>k', { desc = 'Switch to upper window' })
vim.keymap.set('n', '<C-l>', '<C-w>l', { desc = 'Switch to right window' })

-- Quickfix list
vim.keymap.set('n', '[q', vim.cmd.cprev, { desc = 'Previous quickfix item' })
vim.keymap.set('n', ']q', vim.cmd.cnext, { desc = 'Next quickfix item' })

-- Diagnostics
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to prev diagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next diagnostic message' })
vim.keymap.set('n', 'gl', vim.diagnostic.open_float, { desc = 'Open floating diagnostic message' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setqflist, { desc = 'Open diagnostic quickfix list' })

-- Nagivation in insert
vim.keymap.set({'i', 'c'}, '<C-h>', '<Left>', { desc = "Forward char" })
vim.keymap.set({'i', 'c'}, '<C-j>', '<C-o>gj', { desc = "Up char" })
vim.keymap.set({'i', 'c'}, '<C-k>', '<C-o>gk', { desc = "Down char" })
vim.keymap.set({'i', 'c'}, '<C-l>', '<Right>', { desc = "Backward char" })

-- <C-g> to exit
vim.keymap.set({'i', 'n', 'v', 'c'}, '<C-g>', '<Esc>', { desc = "Exit mode" })
vim.cmd('cmap <C-g> <C-c>')

-- C-d to delete
vim.keymap.set('i', '<C-d>', '<Del>', { desc = "Delete forward char"})

-- Jump forward and back
vim.keymap.set('n', '<C-l>', '$', { desc = "Delete forward char"})
vim.keymap.set('n', '<C-h>', '_', { desc = "Delete forward char"})

-- File managerment
vim.keymap.set('n', '<leader>e', ':e <C-R>=expand("%:p:h") . "/"<CR>', { desc = 'Open file in current directory' })

-- Insert at correct tab
vim.keymap.set("n", "i", function()
  local line = vim.api.nvim_get_current_line()
  if #line == 0 then
    return [["_cc]]
  else
    return "i"
  end
end, { expr = true, noremap = true })
