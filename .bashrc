#
# ~/.bashrc
#

[[ $- != *i* ]] && return

[ -r /usr/share/bash-completion/bash_completion ] && . /usr/share/bash-completion/bash_completion

alias cp="cp -i"                          # confirm before overwriting something

xhost +local:root > /dev/null 2>&1

complete -cf sudo

# http://cnswww.cns.cwru.edu/~chet/bash/FAQ (E11)
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s histappend

export TERM="xterm-256color"

source ~/.ehconf

function mr_cool() {
    local RETVAL=$(echo $?)
    local RETVALCOL="\[\e[37;42m\]"

    local TAIL="$(pwd)"
    local TAILCOL="\[\e[37;44m\]"

    local PROMPT="λ\[\e[0m\] "
    local PROMPTCOL="\[\e[00;34m\]"

    # if we're in a git repo, add git stats
    local GITBRANCH=""
    local GITBRANCHCOL=""
    local GITCHANGES=""
    if [ $(git rev-parse --is-inside-work-tree 2> /dev/null) ]; then
        GITBRANCH=$(git branch | grep "*" | sed 's/^..//' | tr a-z A-Z)
        GITBRANCH="  $GITBRANCH"
        GITBRANCHCOL="\[\e[37;42m\]"

        GITCHANGES=$(git status -uno --short $(pwd) | wc -l)

        if [ $GITCHANGES -eq 0 ]; then
            GITCHANGES="  "
        else
            GITCHANGES=" +$GITCHANGES  "
        fi
    fi

    if [ $RETVAL -ne 0 ]; then
        RETVALCOL="\[\e[37;41m\]"
    fi

    export PS1="\\
$RETVALCOL  $RETVAL  \\
$TAILCOL  $TAIL  \\
$GITBRANCHCOL$GITBRANCH$GITCHANGES\[\e[0m\]\\
\n$PROMPTCOL$PROMPT"
}

PROMPT_COMMAND=mr_cool

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
