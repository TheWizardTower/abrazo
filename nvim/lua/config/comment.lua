local comment = require("Comment")
pcall(function()
    require("ts_context_commentstring").setup({})
end)

local function pre_hook(ctx)
    local ok, ts_context = pcall(require, "ts_context_commentstring.integrations.comment_nvim")
    if ok then
        return ts_context.create_pre_hook()(ctx)
    end
end

comment.setup({
    padding = true,
    sticky = true,
    ignore = "^$",
    toggler = {
        line = "gcc",
        block = "gbc",
    },
    opleader = {
        line = "gc",
        block = "gb",
    },
    pre_hook = pre_hook,
})

