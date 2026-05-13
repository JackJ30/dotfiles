return {
    'dnlhc/glance.nvim',
    cmd = 'Glance',
	config = function()
		require("glance").setup({})
	end,
	keys = {
		{ 'gr', '<CMD>Glance references<CR>' }
	}
}
