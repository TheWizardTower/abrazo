# Jujube ("jj") Integration for Neovim

## Current State (August 2025)

As of now, there is **no official Neovim integration library** for Jujube (`jj`), the fast Git-compatible version control system.

### What Exists:

1. **`rafikdraoui/jj-diffconflicts`**
   - GitHub: rafikdraoui/jj-diffconflicts
   - Purpose: Handle merge conflicts in jujube repositories
   - Status: Installed but needs keymaps

2. **Custom Integration (Created Just Now!)**
   - Uses `jj` command directly via Neovim's `:!` and jobstart()
   - Provides keymaps for status, log, and diff operations

## How to Use Your New Jujube Setup:

### Key Mappings:

| Mapping | Action |
|---------|--------|
| `<leader>js` | Show jj status (working copy) |
| `<leader>jg` | View commit history (log) |
| `<leader>jd` | Show diff of changes |

### Manual Commands:

You can also run `jj` commands directly:
```vim
:!jj status           " Full status output
:!jj log --oneline    " Recent commits
:!jj diff             " Changes in working copy
```

## What's Missing for Full Integration:

- ✅ Basic command execution via `:JjStatus`, `:JjLog`
- ⏳ Telescope pickers for jj subcommands
- ⏳ Visual indicators in statusline for jj repo state
- ⏳ DAP integration for jj operations

## Recommended Workflow:

1. **Quick Check**: `<leader>js` - Shows working copy status
2. **Review History**: `<leader>jg` - View recent commits
3. **See Changes**: `<leader>jd` - Review diffs before committing

## Future Enhancements:

If better integration becomes available, consider:
- `bbkno1/jj-telescope.nvim` (if released)
- Official jj Neovim plugin from the jujube team
- Contributing to existing projects

