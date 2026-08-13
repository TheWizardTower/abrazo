# Neovim Lua Code Linting

## Overview

The Neovim configuration includes automated linting in CI to ensure code quality and prevent syntax errors.

## What's Checked

### 1. Parse Check (luac5.1)
- Verifies all Lua files have valid syntax
- Uses lua5.1 which matches Neovim's embedded LuaJIT dialect

### 2. Style Check (StyLua)
- Enforces consistent code formatting
- Runs `stylua --check` to verify formatting without modifying files

### 3. Linting (luacheck)
- Identifies potential bugs and anti-patterns
- Checks for unused variables, undefined globals, etc.
- Configured in `.luacheckrc`

## Running Locally

### Using the lint script:
```bash
./nvim/scripts/lint-lua.sh
```

### Manual commands:

**Parse check:**
```bash
luac5.1 -p nvim/**/*.lua 2>&1 | grep -v "lazy-lock"
```

**Style check:**
```bash
stylua --check nvim/
```

**Linting:**
```bash
cd nvim && luacheck . --config .luacheckrc --exclude lazy-lock
```

## CI Integration

The lint checks run on:
- Every push to master branch
- All pull requests  
- Weekly (Mondays 04:00 UTC) for drift detection

See `.github/workflows/lint.yml` for the complete configuration.

## Configuration Files

| File | Purpose |
|------|---------|
| `nvim/.luacheckrc` | Luacheck linting rules |
| `.github/workflows/lint.yml` | CI pipeline definition |

## Adding New Lua Files

When adding new Lua files to nvim/:

1. Follow the existing style
2. Ensure syntax is valid (test with `luac5.1`)
3. Use proper global variable names (`vim`, `require`, etc.)
4. Consider adding keybindings in which-key if applicable

