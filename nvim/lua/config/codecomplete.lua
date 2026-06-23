require("codecompanion").setup({
  log_level = "DEBUG",
  strategies = {
    chat = {
      adapter = {
        name = "openai",
        model = "Qwen3-Coder-Next-UD-Q8_K_XL",
        api_base = "http://turkishDelight:8080/v1",
        api_key = "not-needed",
      },
    },
  },
})
