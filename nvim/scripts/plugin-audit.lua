-- Plugin Audit Script for Neovim Configuration

local function audit_plugins()
    local vim = vim
    
    print("=== Plugin Audit ===")
    
    local plugins_dir = vim.fn.stdpath("config") .. "/lua/plugins/"
    
    -- Get all plugin files
    local plugin_files = {}
    for _, file in ipairs(vim.fs.find(plugins_dir, { type = "file", limit = 100 })) do
        if file:match("%.lua$") then
            table.insert(plugin_files, file)
        end
    end
    
    print(string.format("Found %d plugin files:", #plugin_files))
    
    -- Analyze each plugin
    for _, file in ipairs(plugin_files) do
        local name = file:gsub(".*/", ""):gsub("%.lua$", "")
        
        -- Read the file to check if it's properly configured
        local f = io.open(file, "r")
        if f then
            local content = f:read("*all")
            f:close()
            
            -- Check for common issues
            local has_setup = content:match("setup%(") or content:match(":setup%(")
            
            print(string.format(
                "  %s - %s",
                name,
                has_setup and "Configured" or "May need setup"
            ))
        end
    end
    
    print("\n=== Recommendations ===")
    print("- Consider removing unused plugins (see full list above)")
    print("- Ensure all plugins have proper event triggers for lazy loading")
end

audit_plugins()
