export PATH=$HOME/bin:/usr/local/bin:$HOME/.gem/ruby/2.6.0/bin:$HOME/flutter/bin:$PATH
export PATH=~/tools/nvim-macos/bin:$PATH
export PATH=/opt/homebrew/opt/llvm/bin:$PATH
export PATH=~/go/bin:$PATH

export CPPFLAGS="-I/opt/homebrew/opt/openjdk/include"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="headline"
export XDG_CONFIG_HOME=$HOME/.config

plugins=(git)

source $ZSH/oh-my-zsh.sh

alias vim=nvim

findandkill() {
  $(lsof -ti:3000 | xargs kill)
}

alias kill_port=findandkill

change_kitty_profile() {
  sh ~/dotfiles/.config/kitty/change-profile.sh
}

toggle_kitty_transparency() {
  sh ~/dotfiles/.config/kitty/toggle-transparency.sh
}

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
eval "$(/opt/homebrew/bin/brew shellenv)"
export PATH="/opt/homebrew/opt/mysql@5.7/bin:$PATH"

# pnpm
export PNPM_HOME="$HOME/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"
# pnpm end

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
export PATH="$HOME/.local/share/sentry-devenv/bin:$PATH"

# Set a blazingly fast keyboard repeat rate
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write NSGlobalDomain InitialKeyRepeat -int 15
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# sst
export PATH=/Users/lazarnikolov/.sst/bin:$PATH

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /opt/homebrew/bin/terraform terraform
