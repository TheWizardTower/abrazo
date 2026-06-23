return {
  {
    "zbirenbaum/copilot.lua",
    config = function()
      require("copilot").setup({})
    end,
  },
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    dependencies = {
      { "zbirenbaum/copilot.lua" },
      { "nvim-lua/plenary.nvim", branch = "master" },
    },
    build = "make tiktoken",
    opts = {
      model = 'Qwen3-Coder-Next-UD-Q8',
      temperature = 0.1,
      providers = {
        turkishDelight = {
          get_url = function(opts) return 'http://turkishDelight:8080/v1/chat/completions' end,
          get_headers = function() return { ['Authorization'] = 'Bearer not-needed' } end,
          get_models = function()
            return { { id = 'Qwen3-Coder-Next-UD-Q8', name = 'turkishDelight Qwen3' } }
          end,
          prepare_input = function(inputs, opts)
            local messages = {}
            for _, input in ipairs(inputs) do
              table.insert(messages, {
                role = input.role,
                content = input.content or (input.images and #input.images > 0 and "Image attached" or ""),
              })
            end
            return {
              model = 'Qwen3-Coder-Next-UD-Q8',
              messages = messages,
              temperature = opts.temperature or 0.1,
            }
          end,
          prepare_output = function(output, opts)
            if output.choices and #output.choices > 0 then
              local msg = output.choices[1]
              if msg.message and msg.message.content then
                return { content = msg.message.content }
              end
            end
            return { content = "" }
          end,
        }
      },
    },
    cmd = "CopilotChat",
    event = "InsertEnter"
  },
}
