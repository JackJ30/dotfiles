return {
	"ya2s/nvim-cursorline",
	opts = {
		cursorline = { enable = false }, -- I don't want this plugin fucking with my cursorline
		cursorword = {
			enable = true,
			min_length = 2,
			hl = { underline = true },
		}
	}
}
