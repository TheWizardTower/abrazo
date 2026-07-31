return {
  -- Jujube ("jj") integration for Neovim
  
  -- jj-diffconflicts is handled by its own plugin file

  -- Custom Jujube Telescope extension (built into Neovim)
  {
    "nvim-telescope/telescope.nvim",
    dependencies = {},
    config = function()
      -- Register custom jj pickers
      local actions = require("telescope.actions")
      
      vim.api.nvim_create_user_command("JjStatus", function(args)
        local job_id = vim.fn.jobstart({"jj", "status"}, {
          on_stdout = function(_, data)
            if data and #data > 0 then
              for _, line in ipairs(data) do
                print(line)
              end
            end
          end,
          on_exit = function()
            print("JJ status completed")
          end
        })
      end, { nargs = "*" })

      vim.api.nvim_create_user_command("JjLog", function(args)
        local args_str = table.concat(args.fargs, " ")
        local job_id = vim.fn.jobstart({"jj", "log"} .. (args_str and " " .. args_str or ""), {
          on_stdout = function(_, data)
            if data and #data > 0 then
              for _, line in ipairs(data) do
                print(line)
              end
            end
          end,
          on_exit = function()
            print("JJ log completed")
          end
        })
      end, { nargs = "*" })
    end,
  },
}
