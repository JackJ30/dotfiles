return {
    "windwp/nvim-autopairs",
    opt = true,
    event = "InsertEnter",
    config = function()
        require("nvim-autopairs").setup()
    end
}
