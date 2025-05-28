return {
    'hrsh7th/nvim-cmp',
    event = { 'InsertEnter', 'CmdlineEnter' },
    dependencies = {
        'hrsh7th/cmp-buffer',
        'hrsh7th/cmp-cmdline',
        'hrsh7th/cmp-path',
    },
    opts = function()
        local cmp = require('cmp')

        return {
            mapping = cmp.mapping.preset.insert(),
            sources = cmp.config.sources({
                { name = 'nvim_lsp' },
            }, {
                { name = 'buffer' },
            }, {
                { name = 'path' },
            })
        }
    end,
}

