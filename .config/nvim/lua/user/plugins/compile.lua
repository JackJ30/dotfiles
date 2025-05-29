return {
	"ej-shafran/compile-mode.nvim",
	dependencies = {
		"nvim-lua/plenary.nvim",
		{ "m00qek/baleia.nvim", tag = "v1.3.0" },
	},
	config = function()
		vim.keymap.set('n', '<leader>cc', ':vert Compile<Cr>', { desc = "Compile"})
		vim.keymap.set('n', '<leader>cr', ':vert Recompile<Cr>', { desc = "Recompile"})
		vim.g.compile_mode = {
			-- to add ANSI escape code support, add:
			baleia_setup = true,
		}
	end
}
