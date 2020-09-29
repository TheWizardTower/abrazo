# These are set here, rather than in paths.d/ because they weren't reliably getting created before this was run.
set -gx SSH_ENV $HOME/.ssh/environment_fish
set -gx SSH_SOCK_LINK $HOME/.ssh/auth_sock_symlink_fish

function start_agent
    fish echo "Initializing new SSH agent ..."
    set -l ssh_eval (/usr/bin/ssh-agent -c | sed 's/setenv/set -gx/g')
    echo $ssh_eval >$SSH_ENV
    echo "succeeded"
    chmod 600 $SSH_ENV
    source $SSH_ENV
    ssh-add ~/.ssh/{aws,github,bitbucket,bitbucket-daisee}
    ln -sf $SSH_AUTH_SOCK $SSH_SOCK_LINK
end

function test_identities
    ssh-add -l | grep -qe "The agent has no identities" >/dev/null
    if test $status -eq 0
        ssh-add ~/.ssh/github ~/.ssh/bitbucket
        if [ $status -eq 2 ]
            start_agent
        end
    end
end

if status --is-interactive
else
  exit
end

set -l output
if test -z (hostname | grep lin[[:alpha:]]-sandbox)
    if test -n "$SSH_AGENT_PID"
        echo "Passed ssh-agent-pid guard."
        ps -ef | grep -q "$SSH_AGENT_PID" | grep -q ssh-agent
        if [ $status -eq 0 ]
            test_identities
        end
    else
        if test -f $SSH_ENV
            source $SSH_ENV >/dev/null
        end
        ps -ef | grep -q $SSH_AGENT_PID | grep -q [s]sh-agent
        if test $status -eq 0
            test_identities
        else
            start_agent
        end
    end
end
