# These are set here, rather than in paths.d/ because they weren't reliably getting created before this was run.
set -gx SSH_ENV $HOME/.ssh/environment
set -gx SSH_SOCK_LINK $HOME/.ssh/auth_sock_symlink

function start_agent
    fish echo "Initializing new SSH agent ..."
    set -l ssh_eval (/usr/bin/ssh-agent -c | sed 's/setenv/set -gx/g')
    echo $ssh_eval >$SSH_ENV
    echo "succeeded"
    chmod 600 $SSH_ENV
    source $SSH_ENV
    ssh-add ~/.ssh/ ~/.ssh/github
    ln -sf $SSH_AUTH_SOCK $SSH_SOCK_LINK
end

function test_identities
    ssh-add -l | grep -e "The agent has no identities" >/dev/null
    if test $status -eq 0
        ssh-add ~/.ssh/github
        if [ $status -eq 2 ]
            start_agent
        end
    end
end


set -l output
if test -z (hostname | grep lin[[:alpha:]]-sandbox)
    if test -n "$SSH_AGENT_PID"
        echo "Passed ssh-agent-pid guard."
        ps -ef | grep "$SSH_AGENT_PID" | grep ssh-agent >/dev/null
        if [ $status -eq 0 ]
            test_identities
        end
    else
        if test -f $SSH_ENV
            source $SSH_ENV >/dev/null
        end
        ps -ef | grep $SSH_AGENT_PID | grep [s]sh-agent >/dev/null
        if test $status -eq 0
            test_identities
        else
            start_agent
        end
    end
end
