if status is-interactive
  fish_vi_key_bindings

  fish_add_path "$HOME"/.ghcup/bin
  fish_add_path "$HOME"/.cargo/bin
  fish_add_path "$HOME"/.local/bin
  fish_add_path "$HOME"/.opam/default/bin
  fish_add_path "$HOME"/Library/Python/3.9/bin:/Library/TeX/texbin
  fish_add_path "$HOME"/bin
  fish_add_path "$HOME"/go/bin
  fish_add_path /Applications/Emacs.app/Contents/MacOS/bin-arm64-11
  fish_add_path /Applications/Emacs.app/Contents/MacOS/libexec-arm64-11
  fish_add_path /bin
  fish_add_path /opt/homebrew/bin
  fish_add_path /opt/homebrew/lib/ruby/gems/3.1.0/bin
  fish_add_path /opt/homebrew/opt/llvm/bin
  fish_add_path /opt/homebrew/opt/node@16/bin
  fish_add_path /opt/homebrew/opt/ruby/bin
  fish_add_path /opt/homebrew/sbin
  fish_add_path /sbin
  fish_add_path /usr/bin
  fish_add_path /usr/local/bin
  fish_add_path /usr/sbin

  alias aliases='nvim ~/.config/fish/config.fish'
  alias c='clear'
  alias dots='nvim ~/.config/homer/dotfiles.dots'
  alias e='exit'
  alias fishrc='nvim ~/.config/fish/config.fish'
  alias l='exa -l'
  alias lh='exa -l -a'
  alias ls='exa -l'
  alias reload='source ~/.config/fish/config.fish'
  alias v='nvim'
  alias vi='nvim'
  alias vim='nvim'
  alias sc='pwd > ~/.config/emacs/current-project'
  alias gp='cd (cat ~/.config/emacs/current-project)'
  alias rimraf='rm -rf'
  alias ggh='cd ~/code/github'
  alias gjh='cd ~/code/github/joshuaharry'
  gp
end
