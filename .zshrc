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

. /usr/share/zsh/site-contrib/powerline.zsh
