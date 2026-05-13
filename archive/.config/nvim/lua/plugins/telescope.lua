return {
    'nvim-telescope/telescope.nvim',
    cmd = 'Telescope',
    dependencies = {
        'nvim-lua/plenary.nvim',
    },
    keys = function()
		local lazy_telescope = function(builtin)
			return function(...)
				if builtin == 'find_files_custom' then
					require('telescope.builtin').find_files({ find_command = {'rg', '--files', '--hidden', '-g', '!.git', '-g', '!external/', '-g', '!build/' }})
				else
					require('telescope.builtin')[builtin](...)
				end
			end
		end
        return {
            { '<leader>fb', lazy_telescope('buffers'),                   desc = 'Find buffers' },
            { '<leader>fd', lazy_telescope('diagnostics'),               desc = 'Find diagnostics' },
            { '<leader>fF', lazy_telescope('git_files'),                 desc = 'Find Git files' },
            { '<leader>fG', lazy_telescope('git_status'),                desc = 'Find Git status' },
            { '<leader>ff', lazy_telescope('find_files_custom'),         desc = 'Find files' },
            { '<leader>fg', lazy_telescope('live_grep'),                 desc = 'Find files by content' },
            { '<leader>fh', lazy_telescope('help_tags'),                 desc = 'Find help tags' },
            { '<leader>fo', lazy_telescope('oldfiles'),                  desc = 'Find recently opened files' },
            { '<leader>fw', lazy_telescope('grep_string'),               desc = 'Find word in buffer' },
            { '<leader>f/', lazy_telescope('current_buffer_fuzzy_find'), desc = 'Find fuzzy match in current buffer' },
        }
    end,
	config = function()
        local telescope = require('telescope')
		telescope.setup({
            defaults = {
                mappings = {
                    i = {
                        ["<C-g>"] = require('telescope.actions').close,
                    },
                    n = {
                        ["<C-g>"] = require('telescope.actions').close,
                    },
                },
            },
        })
    end,
}
