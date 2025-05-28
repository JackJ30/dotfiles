return {
	"NAHTAIV3L/vertico.nvim",
	dependencies = {
		'nvim-lua/plenary.nvim',
	},
	config = function ()
		vim.keymap.set("n", "<leader>f", require("vertico").find_file)
		vim.keymap.set("n", "<C-g>", require("vertico").close)
	end
}
