#!/usr/bin/env zsh
eval "$(/opt/homebrew/bin/brew shellenv)"
export EDITOR="$(if [ $TERM = xterm-256color ]; then printf "%s" vim-if-not-emacs; else printf "%s" nvim; fi)"
export PATH=/opt/homebrew/opt/llvm/bin:\
/opt/homebrew/opt/node@16/bin:\
/opt/homebrew/opt/ruby/bin:\
/opt/homebrew/lib/ruby/gems/3.1.0/bin:\
/opt/homebrew/bin:\
/opt/homebrew/sbin:\
~/.opam/default/bin:\
/usr/local/bin:\
/usr/bin:\
/bin:\
/usr/sbin:\
/sbin:\
~/bin:\
~/.local/bin:\
~/go/bin:\
/Applications/Emacs.app/Contents/MacOS/bin-arm64-11:\
/Applications/Emacs.app/Contents/MacOS/libexec-arm64-11:\
~/Library/Python/3.9/bin:\
/Library/TeX/texbin:\
~/.ghcup/bin
