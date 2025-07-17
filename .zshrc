export PATH=$HOME/bin:/usr/local/bin:$HOME/.gem/ruby/2.6.0/bin:$HOME/flutter/bin:$HOME/.local/bin/:$PATH
export PATH=~/tools/nvim-macos/bin:$PATH
export PATH=/opt/homebrew/opt/llvm/bin:$PATH
export PATH=~/go/bin:$PATH
export PATH=~/.config/composer/vendor/bin:$PATH

export CPPFLAGS="-I/opt/homebrew/opt/openjdk/include"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"

# Python & pyenv
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="headline"
export XDG_CONFIG_HOME=$HOME/.config

if [[ -z "$ZSH_VERSION_LOADED" ]]; then
  export ZSH_VERSION_LOADED=1
  source $ZSH/oh-my-zsh.sh
fi

export NVIM_LISTEN_ADDRESS=/tmp/nvim-nikolovlazar-$$.sock
alias vim=nvim

findandkill() {
  $(lsof -ti:3000 | xargs kill)
}

alias kill_port=findandkill

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

# asdf shims
export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"

PATH=~/.console-ninja/.bin:$PATH

# check if direnv exists before executing it
if command -v direnv &> /dev/null; then
  export PATH="$HOME/.local/share/sentry-devenv/bin:$PATH"
  eval "$(direnv hook zsh)"
fi

if [ -f "$HOME/dotfiles/environment.sh" ]; then
  source ~/dotfiles/environment.sh
fi

# uv
. "$HOME/.local/bin/env"
