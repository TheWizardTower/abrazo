# Fzf integration for nushell with fallback handling

# Custom fzf completer that works with nushell's external command system
export def "fzf_completer" [] {
    {|spans|
        # Try carapace first (my existing universal completer)
        load-env {
            CARAPACE_SHELL_BUILTINS: (help commands | where category != "" | get name | each { split row " " | first } | uniq | str join "\n")
            CARAPACE_SHELL_FUNCTIONS: (help commands | where category == "" | get name | each { split row " " | first } | uniq | str join "\n")
        }

        let expanded_alias = scope aliases | where name == $spans.0 | first | get expansion? | default $spans.0

        let spans = if $expanded_alias != null {
            $spans | skip 1 | prepend ($expanded_alias | split row " " | take 1)
        } else {
            $spans | skip 1 | prepend $spans.0
        }

        # Try carapace
        let result = try {
            carapace $spans.0 nushell ...$spans | from json
        } catch { [] }

        if ($result | len) > 0 {
            $result
        } else {
            # Fallback: basic path completion for any file path
            let last_part = $spans | last
            let path_info = (
                if ($"/($last_part)" | path type) == "dir" {
                    { base: $last_part, pattern: "*" }
                } else {
                    { base: ($last_part | path dirname), pattern: ($last_part | path basename) }
                }
            )

            let base = if $path_info.base == "." { "" } else { $path_info.base }
            let dir_exists = (
                if $base == "" {
                    true
                } else {
                    $"/($base)" | path exists
                }
            )
            if $dir_exists {
                ls $base
                | where name =~ ($path_info.pattern + ".*")
                | each {|f|
                    if $f.is_dir {
                        { display: $"($f.name)/", value: $"($f.name)/" }
                    } else {
                        { display: $f.name, value: $f.name }
                    }
                }
            } else {
                []
            }
        }
    }
}
