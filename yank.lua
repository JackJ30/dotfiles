{
	"svban/YankAssassin.nvim",
	event = "VeryLazy",
	config = function()
		require("YankAssassin").setup {
			auto_normal = true, -- if true, autocmds are used. Whenever y is used in normal mode, the cursor doesn't move to start
			auto_visual = true, -- if true, autocmds are used. Whenever y is used in visual mode, the cursor doesn't move to start
		}
	end,
}
