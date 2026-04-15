let carapace_completer = {|spans|
  load-env {
    CARAPACE_SHELL_BUILTINS: (help commands | where category != "" | get name | each { split row " " | first } | uniq  | str join "\n")
    CARAPACE_SHELL_FUNCTIONS: (help commands | where category == "" | get name | each { split row " " | first } | uniq  | str join "\n")
  }

  let expanded_alias = (scope aliases | where name == $spans.0 | $in.0?.expansion?)

  let spans = (if $expanded_alias != null  {
    $spans | skip 1 | prepend ($expanded_alias | split row " " | take 1)
  } else {
    $spans | skip 1 | prepend ($spans.0)
  })

  carapace $spans.0 nushell ...$spans
  | from json
}

mut current = (($env | default {} config).config | default {} completions)
$current.completions = ($current.completions | default {} external)
$current.completions.external = ($current.completions.external
| default true enable
| upsert completer { if $in == null { $carapace_completer } else { $in } })

$env.config = $current
