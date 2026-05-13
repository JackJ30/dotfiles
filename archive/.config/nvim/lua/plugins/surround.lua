return {
    "echasnovski/mini.surround",
    event = "VeryLazy",
    config = function()
        require("mini.surround").setup({
			mappings = {
				add = '',
				delete = 'ds',
				find = '',
				find_left = '',
				highlight = '',
				replace = '',
				update_n_lines = '',

				-- Add this only if you don't want to use extended mappings
				suffix_last = '',
				suffix_next = '',
			}
        })
    end
}
