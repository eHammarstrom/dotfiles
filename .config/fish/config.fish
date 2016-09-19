set -x GOPATH $HOME/Go
set -x VISUAL vim
set -x EDITOR $VISUAL

alias mysqlwebshop 'mysql -p -h localhost -D webshop'
alias td "$HOME/scripts/tmux_dev.sh"
