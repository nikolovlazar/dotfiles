##### ── Core environment (safe for non‑interactive shells) ─────────────────────

# Base config roots
export XDG_CONFIG_HOME="$HOME/.config"

# Toolchain flags
export CPPFLAGS="-I/opt/homebrew/opt/openjdk/include -I/opt/homebrew/opt/llvm/include"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"

# Homebrew (quiet)
eval "$(/opt/homebrew/bin/brew shellenv)" 2>/dev/null

# PATH (grouped; duplicates removed)
export PATH="$HOME/bin:/usr/local/bin:$HOME/.local/bin:$PATH"
export PATH="$HOME/tools/nvim-macos/bin:$PATH"
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.config/composer/vendor/bin:$PATH"
export PATH="/opt/homebrew/opt/mysql@5.7/bin:$PATH"
export PATH="$HOME/.local/share/sentry-devenv/bin:$PATH"
export PATH="$HOME/.pyenv/bin:$PATH"

# uv (only if present; quiet)
[ -f "$HOME/.local/bin/env" ] && . "$HOME/.local/bin/env" >/dev/null 2>&1

##### ── Stop here for non‑interactive shells (Cursor, LSPs, scripts) ──────────
[[ $- == *i* ]] || return

##### ── Interactive‑only configuration ─────────────────────────────────────────

# Oh My Zsh
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="headline"
source "$ZSH/oh-my-zsh.sh"

# Neovim + aliases
export NVIM_LISTEN_ADDRESS="/tmp/nvim-nikolovlazar-$$.sock"
alias vim='nvim'

# quick port killer (usage: kill_port 3000)
findandkill() { lsof -ti:"${1:-3000}" | xargs kill 2>/dev/null; }
alias kill_port='findandkill'

# Apps
alias tailscale="/Applications/Tailscale.app/Contents/MacOS/Tailscale"
alias codex_super="codex --sandbox danger-full-access -m gpt-5-codex -c model_reasoning_effort=\"high\" --enable web_search_request"

# Serena
alias serena="uvx --from git+https://github.com/oraios/serena serena"

# pnpm
export PNPM_HOME="$HOME/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"

# direnv (if installed)
if command -v direnv &>/dev/null; then
  export PATH="$HOME/.local/share/sentry-devenv/bin:$PATH"
  eval "$(direnv hook zsh)"
fi

# Private/local environment
[ -f "$HOME/dotfiles/environment.sh" ] && source "$HOME/dotfiles/environment.sh"

# Activate mise
eval "$(mise activate zsh)"

# ❗If you want macOS key‑repeat tweaks, run once manually (don’t keep in .zshrc):
# defaults write NSGlobalDomain KeyRepeat -int 1
# defaults write NSGlobalDomain InitialKeyRepeat -int 15
# defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Activate mise environment
eval "$(mise activate zsh)"

# Added by Antigravity
export PATH="$HOME/.antigravity/antigravity/bin:$PATH"
