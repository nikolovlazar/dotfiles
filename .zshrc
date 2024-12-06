# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/powerlevel10k/powerlevel10k.zsh-theme

plugins=(
    git
    zsh-autosuggestions
    web-search
    sudo
    copypath
    copyfile
    copybuffer
    dirhistory
    history
    macos    
    jsontools
    zsh-syntax-highlighting
    fig
)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='nvim'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
alias ls="lsd"
alias myip='ipconfig getifaddr en0'
alias py='python'
alias ipy='ipython'
alias cls='clear'
alias c='clear'
alias rm="trash"
alias l="ls -al"
alias nivm="nvim"

    # exit="exit"
alias e="exit"
alias exti="exit"
alias etix="exit"
alias etxi="exit"
alias eixt="exit"
alias eitx="exit"
alias xiet="exit"
alias xite="exit"
alias xeit="exit"
alias xeti="exit"
alias xtei="exit"
alias xtie="exit"
alias ixet="exit"
alias ixte="exit"
alias itex="exit"
alias itxe="exit"
alias iext="exit"
alias ietx="exit"
alias tixe="exit"
alias tiex="exit"
alias texi="exit"
alias teix="exit"
alias txei="exit"
alias txie="exit"

alias p='pnpm'

[ "$TERM" = "xterm-kitty" ] && alias ssh="kitty +kitten ssh"


# A smarter cd command.
eval "$(zoxide init zsh)"
alias cd="z"



# command prompt starship 
# eval "$(starship init zsh)"
# I use command prompt powerlevel10k instead for now

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export NVM_DIR="$HOME/.nvm"
export PATH="$PATH/Users/zrxmax/.elasticsearch/bin" # elasticsearch
# export PATH="$PATH:/Users/zrxmax/env/py/py312/bin" # python
export PATH="$PATH:/Users/zrxmax/bin/mongosh"           # mongosh
export PATH="$PATH:/Users/zrxmax/env/py/py312/bin"
export PATH="$PATH:/Users/zrxmax/.local/bin"
export PATH="$PATH:/Users/zrxmax/go/bin"
export PATH="$PATH:/Users/zrxmax/dev/depot_tools"
export LANG="en_US.UTF-8 LC_CTYPE=en_US.UTF-8"
export PIXI_COLOR=always

# nvm
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
# [ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"export PATH="/Users/zrxmax/.nvm/versions/node/v20.9.0/lib/node_modules/.bin:$PATH"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
nvm use v20 > /dev/null 2>&1
# nvm end

# pnpm
export PNPM_HOME="/Users/zrxmax/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

# bun completions
[ -s "/Users/zrxmax/.bun/_bun" ] && source "/Users/zrxmax/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
. "/Users/zrxmax/.deno/env"
