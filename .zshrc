#!/usr/bin/env zsh

ZINIT_PATH=$HOME/.zinit
if [ ! -d "$ZINIT_PATH/bin" ]; then
    git clone https://github.com/zdharma-continuum/zinit $ZINIT_PATH/bin
fi

source "$ZINIT_PATH"/bin/zinit.zsh
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma/fast-syntax-highlighting
zinit light ael-code/zsh-colored-man-pages
zinit load agkozak/agkozak-zsh-prompt
# Unfortunately, zinit changes the path in an undesired way; change it back
# by reloading our .zprofile init.
source "$HOME"/.zprofile

if ! [ "$TERM" = xterm-256color ]; then
    set -o vi
fi

source "$HOME"/.aliases.sh
export ELDEV_EMACS=emacs
setopt auto_cd
