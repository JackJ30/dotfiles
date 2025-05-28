return {
    'nvim-telescope/telescope.nvim',
    cmd = 'Telescope',
    dependencies = {
        'nvim-lua/plenary.nvim',
		{ 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    },
    keys = function()
        local lazy_telescope = function(builtin)
            return function(...)
                require('telescope.builtin')[builtin](...)
            end
        end
        return {
            { '<leader>fb', lazy_telescope('buffers'),                   desc = 'Find buffers' },
            { '<leader>fd', lazy_telescope('diagnostics'),               desc = 'Find diagnostics' },
            { '<leader>ff', lazy_telescope('git_files'),                 desc = 'Find Git files' },
            { '<leader>fF', lazy_telescope('find_files'),                desc = 'Find files' },
            { '<leader>fg', lazy_telescope('live_grep'),                 desc = 'Find files by content' },
            { '<leader>fh', lazy_telescope('help_tags'),                 desc = 'Find help tags' },
            { '<leader>fo', lazy_telescope('oldfiles'),                  desc = 'Find recently opened files' },
            { '<leader>fw', lazy_telescope('grep_string'),               desc = 'Find word in buffer' },
            { '<leader>f/', lazy_telescope('current_buffer_fuzzy_find'), desc = 'Find fuzzy match in current buffer' },
        }
    end,
	opts = function()
        return {
            extensions = {
                fzf = {
                    fuzzy = true,
                    override_generic_sorter = true,
                    override_file_sorter = true,
                    case_mode = 'smart_case',
                }
            },
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
		telescope.load_extension('fzf')
    end,
}
