return {
    'nvim-treesitter/nvim-treesitter',
    name = 'treesitter',
    build = ':TSUpdate',
    config = function()
        require('nvim-treesitter.configs').setup({
            ensure_installed = {
                "vimdoc", "c", "lua", "cpp",
                "bash", "css", "glsl", "nasm", "haskell",
                "hyprlang", "zig",
            },
            sync_install = false,

            auto_install = true,

            indent = {
                enable = true
            },

            highlight = {
                enable = true,

                additional_vim_regex_highlighting = { "markdown" },
            },
        })
    end
}
