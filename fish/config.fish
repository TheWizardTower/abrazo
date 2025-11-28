if status is-interactive
    # Commands to run in interactive sessions can go here
    atuin init fish | source
end

set -q KREW_ROOT; and set -gx PATH $PATH $KREW_ROOT/.krew/bin; or set -gx PATH $PATH $HOME/.krew/bin

# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/home/merlin/.opam/opam-init/init.fish' && source '/home/merlin/.opam/opam-init/init.fish' >/dev/null 2>/dev/null; or true
# END opam configuration

alias kubectl=kubecolor
flox activate --dir ~/ --trust | source
