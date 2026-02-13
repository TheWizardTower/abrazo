require("codecompanion").setup({
  log_level = "DEBUG",
  strategies = {
    chat = {
      adapter = {
        name = "opencode",
        model = "Qwen3-next-80B-A3B-Instruct-UD-Q4-K-XL",
      },
    },
  },
})
