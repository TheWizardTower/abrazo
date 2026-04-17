# History tuning — match HISTSIZE/HISTFILESIZE from bashrc, switch to sqlite
$env.config.history.max_size = 100000
$env.config.history.file_format = "sqlite"
$env.config.history.sync_on_enter = true
$env.config.history.isolation = false
