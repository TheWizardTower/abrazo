
local wk = require("which-key")
wk.add({
    { "<Leader>f",  group = "file" }, -- group
    { "<Leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File", mode = "n" },
    { "<Leader>fb", function() print("hello") end,   desc = "Foobar" },
    { "<Leader>fn", desc = "New File" },
    { "<Leader>f1", hidden = true },                                      -- hide this keymap
    { "<Leader>w",  proxy = "<c-w>",                 group = "windows" }, -- proxy to window mappings
    {
        "<Leader>b",
        group = "buffers",
        expand = function()
            return require("which-key.extras").expand.buf()
        end
    },
    {
        -- Nested mappings are allowed and can be added in any order
        -- Most attributes can be inherited or overridden on any level
        -- There's no limit to the depth of nesting
        mode = { "n", "v" },                          -- NORMAL and VISUAL mode
        { "<Leader>q", "<cmd>q<cr>", desc = "Quit" }, -- no need to specify mode since it's inherited
        { "<Leader>w", "<cmd>w<cr>", desc = "Write" },
    }
})

