#!/usr/bin/env nu
# Regenerate cached nushell completions for tools with native support.
# Autoload files in ./autoload/ source ~/.cache/<tool>/completions.nu at parse time,
# so hosts without a given binary get an empty stub rather than a parse error.
#
# Run manually after installing new tools. To extend, verify the tool's output
# actually starts with `export extern` / `module` / `extern` before adding here —
# many CLIs accept the arguments silently and emit nothing (or bash) for unknown shells.

const tools = [
    [name,     args];
    [atuin,    [gen-completions --shell nushell]]
    [jj,       [util completion nushell]]
    [just,     [--completions nushell]]
    [starship, [completions nushell]]
]

for tool in $tools {
    let cache_dir = ([$env.HOME .cache $tool.name] | path join)
    let cache_file = ([$cache_dir completions.nu] | path join)
    mkdir $cache_dir
    if (which $tool.name | is-empty) {
        "" | save -f $cache_file
        print $"skip  ($tool.name) — not on PATH, wrote empty stub"
    } else {
        ^$tool.name ...$tool.args | save -f $cache_file
        print $"ok    ($tool.name)"
    }
}
