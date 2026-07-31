-- Startup time optimization settings

-- Enable startup time tracking (via vim-startuptime plugin)
vim.g.startuptime_tries = 10

-- Optimize Lua module loading
package.path = vim.fn.stdpath("config") .. "/lua/?.lua;" .. package.path

