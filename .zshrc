# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=2000
SAVEHIST=2000
setopt appendhistory
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/initiumdoeslinux/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

alias negblogdev='cd ~/rails_workspace/dev_projects/neg_blog/'
alias eahblog='cd ~/rails_workspace/prod_projects/eahdev_blog/'

. /usr/share/zsh/site-contrib/powerline.zsh
PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
