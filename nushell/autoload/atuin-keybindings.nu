# Atuin history behavior matching bash:
# - C-n/C-p/down arrow: scroll through history one item at a time (via menu navigation)
# - C-r: launch atuin interactive search UI

$env.config = ($env | default {} config).config
$env.config = ($env.config | default [] keybindings)

$env.config = (
    $env.config | upsert keybindings (
        $env.config.keybindings
        | append {
            name: atuin_down
            modifier: none
            keycode: down
            mode: [emacs, vi_normal, vi_insert]
            event: {
                until: [
                    {send: menudown}
                    {send: executehostcommand cmd: "commandline edit (run-external atuin search '--shell-down-key-binding' | str trim)" }
                ]
            }
        }
    )
)

$env.config = (
    $env.config | upsert keybindings (
        $env.config.keybindings
        | append {
            name: atuin_up
            modifier: none
            keycode: up
            mode: [emacs, vi_normal, vi_insert]
            event: {
                until: [
                    {send: menuup}
                    {send: executehostcommand cmd: "commandline edit (run-external atuin search '--shell-up-key-binding' | str trim)" }
                ]
            }
        }
    )
)

$env.config = (
    $env.config | upsert keybindings (
        $env.config.keybindings
        | append {
            name: atuin_ctrl_p
            modifier: control
            keycode: char_p
            mode: [emacs, vi_normal, vi_insert]
            event: {
                until: [
                    {send: menuup}
                    {send: executehostcommand cmd: "commandline edit (run-external atuin search '--shell-up-key-binding' | str trim)" }
                ]
            }
        }
    )
)

$env.config = (
    $env.config | upsert keybindings (
        $env.config.keybindings
        | append {
            name: atuin_ctrl_n
            modifier: control
            keycode: char_n
            mode: [emacs, vi_normal, vi_insert]
            event: {
                until: [
                    {send: menudown}
                    {send: executehostcommand cmd: "commandline edit (run-external atuin search '--shell-down-key-binding' | str trim)" }
                ]
            }
        }
    )
)

$env.config = (
    $env.config | upsert keybindings (
        $env.config.keybindings
        | append {
            name: atuin_search
            modifier: control
            keycode: char_r
            mode: [emacs, vi_normal, vi_insert]
            event: { send: executehostcommand cmd: "commandline edit (run-external atuin search --interactive | str trim)" }
        }
    )
)
