#!/bin/sh
alias c='clear'
alias l='exa -l'
alias ls='exa -l'
alias lh='exa -l -a'
alias reload='unalias -a && . "$HOME"/.aliases.sh'
alias nvim=vim-if-not-emacs
alias vim=vim-if-not-emacs
alias vi=vim-if-not-emacs
alias v=vim-if-not-emacs
alias sc='pwd > ~/.config/emacs/current-project'
alias gp='cd $(cat ~/.config/emacs/current-project)'
alias rimraf='rm -rf'
