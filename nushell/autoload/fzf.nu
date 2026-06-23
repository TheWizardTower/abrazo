# Fzf integration for nushell with fallback handling

# Custom fzf completer that works with nushell's external command system
let fzf_completer = {|spans|
  # Try carapace first (my existing universal completer)
  load-env {
    CARAPACE_SHELL_BUILTINS: (help commands | where category != "" | get name | each { split row " " | first } | uniq  | str join "\n")
    CARAPACE_SHELL_FUNCTIONS: (help commands | where category == "" | get name | each { split row " " | first }| uniq  | str join "\n")
  }

  let expanded_alias = (scope aliases | where name == $spans.0 | $in.0?.expansion?)

  let spans = (if $expanded_alias != null {
    $spans | skip 1 | prepend ($expanded_alias | split row " " | take 1)
  } else {
    $spans | skip 1 | prepend ($spans.0)
  })

  # Try carapace
 let result = try { carapace $spans.0 nushell ...$spans | from json } catch { [] }
  
 if ($result | len) > 0 {
   $result
 } else {
   # Fallback: basic path completion for ~ and filepaths
   let last_part = $spans | last
   
    if ($last_part | str starts-with "~") or ($last_part | str starts-with "/") or ($last_part | str starts-with ".") {
      let path_info = (
        if ($"/($last_part)" | path type) == "dir" {
          { base: $last_part, pattern: "*" }
        } else {
          { base: ($last_part | path dirname), pattern: ($last_part | path basename) }
        }
      )
      
      let dir_check = (if $path_info.base == "." { true } else { $"/($path_info.base)" | path exists })
      if $dir_check {
        let dir = if $path_info.base == "." { "" } else { $path_info.base }
        ls $dir
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
   } else {
     []
   }
 }
}

# Custom fzf-based history picker
let atuin_fzf = {|query: string|
  # Use atuin search with fzf preview and selection
  atuin search --query $query --shell-upward-binding-command | lines 
  | get from_last_command 
  | each { split row "→" | last | str trim }
  | unique
  | fzf --preview 'echo {}' -m
}

# Fzf file picker with cd support
let fzf_cd = {|pattern: string|
  let matches = (
    if $pattern == "" {
      fd --type d ~ 2>/dev/null
    } else {
      fd --type d ~ 2>/dev/null | where name =~ ($pattern + ".*")
    }
  )
  
  if ($matches | len) > 0 {
    let selected = ($matches | fzf --preview 'ls -la {}' -1)
    if $selected != "" {
      cd $selected
    }
  } else {
    print "No directories found"
  }
}

# Fzf file picker for opening
let fzf_open = {|pattern: string|
  let matches = (
    if $pattern == "" {
      fd --type f . ~ 2>/dev/null | head -1000
    } else {
      fd --type f . ~ 2>/dev/null | where name =~ ($pattern + ".*") | head -1000
    }
  )
  
  if ($matches | len) > 0 {
    let selected = ($matches | fzf --preview 'cat {}' -1)
    if $selected != "" {
      open $selected
    }
  } else {
    print "No files found"
  }
}

# Set up keybindings and shell integration
 mut current = (($env | default {} config).config | default {} completions)
$current.completions = ($current.completions | default {} external)
$current.completions.external = ($current.completions.external
| default true enable
| upsert completer { if $in == null { $fzf_completer } else { $in } })

$env.config = $current

# Export for use in bash/other shells
$env.FZF_COMPLETION_TRIGGER = "**"