return {
    "greggh/claude-code.nvim",
    dependencies = {
        "nvim-lua/plenary.nvim",
    },
    config = function()
        require("claude-code").setup({
            llm = {
                name = "openai",
                api_base = "http://turkishDelight:8080/v1",
                api_key = "not-needed",
                model = "auto",
            },
        })
    end
}
