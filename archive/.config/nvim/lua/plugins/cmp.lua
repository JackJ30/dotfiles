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
		local select_opts = {behavior = cmp.SelectBehavior.Select}

		return {
			snippet = {
				expand = function(args)
					require('luasnip').lsp_expand(args.body)
				end,
			},
			mapping = {
				['<C-p>'] = cmp.mapping.select_prev_item(select_opts),
				['<C-n>'] = cmp.mapping.select_next_item(select_opts),
				['<C-u>'] = cmp.mapping.scroll_docs(-4),
				['<C-d>'] = cmp.mapping.scroll_docs(4),
				['<C-e>'] = cmp.mapping.abort(),
				['<Tab>'] = cmp.mapping.confirm({select = true}),
			},
			sources = cmp.config.sources({
				{ name = 'nvim_lsp', max_item_count = 5, keyword_length = 1 },
				{ name = "luasnip", max_item_count = 5, keyword_length = 2 },
			}, {
				{ name = 'buffer', max_item_count = 2, keyword_length = 3 },
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

