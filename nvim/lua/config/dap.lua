local ok_dap, dap = pcall(require, "dap")
if not ok_dap then
    vim.notify("nvim-dap not found", vim.log.levels.WARN)
    return
end

local ok_dapui, dapui = pcall(require, "dapui")
if not ok_dapui then
    vim.notify("nvim-dap-ui not found", vim.log.levels.WARN)
    return
end

require("nvim-dap-virtual-text").setup({
    commented = true,
})

dapui.setup({
    controls = {
        enabled = true,
        element = "repl",
        icons = {
            pause = "",
            play = "",
            step_into = "",
            step_over = "",
            step_out = "",
            step_back = "",
            run_last = "↻",
            terminate = "□",
        },
    },
})

require("mason-nvim-dap").setup({
    ensure_installed = { "python", "codelldb", "bash" },
    automatic_installation = true,
    handlers = {},
})

dap.listeners.after.event_initialized["dapui_config"] = function()
    dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
    dapui.close()
end
dap.listeners.before.event_exited["dapui_config"] = function()
    dapui.close()
end

local map = function(mode, lhs, rhs, desc)
    vim.keymap.set(mode, lhs, rhs, { silent = true, desc = desc })
end

map("n", "<F5>", dap.continue, "DAP Continue")
map("n", "<F10>", dap.step_over, "DAP Step Over")
map("n", "<F11>", dap.step_into, "DAP Step Into")
map("n", "<F12>", dap.step_out, "DAP Step Out")
map("n", "<Leader>db", dap.toggle_breakpoint, "Toggle Breakpoint")
map("n", "<Leader>dB", function()
    dap.set_breakpoint(vim.fn.input("Breakpoint condition: "))
end, "Conditional Breakpoint")
map("n", "<Leader>dl", dap.run_last, "Run last session")
map("n", "<Leader>dr", dap.repl.toggle, "Toggle REPL")
map("n", "<Leader>du", dapui.toggle, "Toggle DAP UI")

