return {
	'hrsh7th/nvim-cmp',
	event = { 'InsertEnter', 'CmdlineEnter' },
	dependencies = {
		'saadparwaiz1/cmp_luasnip',
		'hrsh7th/cmp-buffer',
		'hrsh7th/cmp-cmdline',
		'hrsh7th/cmp-path',
	},
	opts = function()
		local cmp = require('cmp')

		return {
			snippet = {
				expand = function(args)
					require('luasnip').lsp_expand(args.body)
				end,
			},
			mapping = cmp.mapping.preset.insert(),
			sources = cmp.config.sources({
				{ name = 'nvim_lsp', max_item_count = 5 },
				{ name = "luasnip", max_item_count = 5 },
			}, {
				{ name = 'buffer', max_item_count = 2 },
			}, {
				{ name = 'path', max_item_count = 2 },
			}),
			sorting = {
				comparators = {
					cmp.config.compare.offset,
					cmp.config.compare.exact,
					--cmp.config.compare.scopes,
					cmp.config.compare.score,

					cmp.config.compare.locality,
					cmp.config.compare.kind,
					--cmp.config.compare.sort_text,
					cmp.config.compare.length,
					cmp.config.compare.order,
				},
			},
		}
	end,
}

