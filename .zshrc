# Linux workstation shell config for the Docker dev container.

export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR="nvim"
export VISUAL="nvim"

export PNPM_HOME="$HOME/.local/share/pnpm"
export BUN_INSTALL="$HOME/.bun"
export PATH="$HOME/.local/bin:$HOME/bin:$PNPM_HOME:$BUN_INSTALL/bin:$HOME/go/bin:$PATH"

alias vim="nvim"
alias vi="nvim"
alias ll="ls -la"

kill_port() {
  if [ -z "${1:-}" ]; then
    echo "usage: kill_port <port>"
    return 1
  fi

  if ! command -v lsof >/dev/null 2>&1; then
    echo "lsof is not installed in this container"
    return 1
  fi

  lsof -ti:"$1" | xargs -r kill
}

[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
[ -f "$HOME/.zshrc.local" ] && source "$HOME/.zshrc.local"
