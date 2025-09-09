function fish_hybrid_key_bindings --description \
"Vi-style bindings that inherit emacs-style bindings in all modes"
    for mode in default insert visual
        fish_default_key_bindings -M $mode
    end
    fish_vi_key_bindings --no-erase
end
set --universal fish_key_bindings fish_hybrid_key_bindings

bind '#' __budspencer_toggle_symbols
bind -M visual '#' __budspencer_toggle_symbols
bind ' ' __budspencer_toggle_pwd
bind -M visual ' ' __budspencer_toggle_pwd
bind L __budspencer_cd_next
bind H __budspencer_cd_prev
bind m mark
bind M unmark
bind . __budspencer_edit_commandline
bind -M insert \r __budspencer_preexec
bind \r __budspencer_preexec


