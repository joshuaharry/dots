#!/usr/bin/env zsh

ZINIT_PATH=$HOME/.zinit
if [ ! -d "$ZINIT_PATH/bin" ]; then
    git clone https://github.com/zdharma-continuum/zinit $ZINIT_PATH/bin
fi

source "$ZINIT_PATH"/bin/zinit.zsh
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma/fast-syntax-highlighting
zinit light ael-code/zsh-colored-man-pages
zinit ice pick"async.zsh" src"pure.zsh" # with zsh-async library that's bundled with it.
zinit light sindresorhus/pure

# Unfortunately, zinit changes the path in an undesired way; change it back
# by reloading our .zprofile init.
source "$HOME"/.zprofile

if ! [ "$TERM" = xterm-kitty ]; then
    set -o emacs
fi

source "$HOME"/.aliases.sh
export ELDEV_EMACS=emacs
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib -Wl,-rpath,/opt/homebrew/opt/llvm/lib"
setopt auto_cd
set -o vi
eval "$(direnv hook zsh)"
