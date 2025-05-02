return {
    "lukas-reineke/indent-blankline.nvim",
	event = { "VeryLazy" },
	config = function() 
		vim.opt.list = true
		vim.opt.listchars = {
			tab = "| ",
		}
		require("ibl").setup({
			indent = {
				tab_char = "|",
				char = "|"
			}
		})
	end,
}
