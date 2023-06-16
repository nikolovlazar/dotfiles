# Lazar's dotfiles ✨💽

This repository contains my ever-evolving dotfiles. Check them out! If you find something useful, feel free to add it to your own dotfiles.

![Screenshot](https://i.imgur.com/O2ilEQk.png)

## Neovim Plugins

- [Catppuccin Mocha](https://github.com/catppuccin/nvim) (colorscheme)
- [packer.nvim](https://github.com/wbthomason/packer.nvim) (plugin manager)
- [GitHub Copilot](https://github.com/github/copilot.vim) (plugin for GitHub Copilot, remove this if you don't want it)
- [Neovim Telescope](https://github.com/nvim-telescope/telescope.nvim) (fuzzy finder, used for searching files, buffers, keymaps, etc.)
- [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) (code parser, needed for imroved syntax highlighting)
- [harpoon](https://github.com/theprimeagen/harpoon) (quickly jump between files)
- [undotree](https://github.com/mbbill/undotree) (visualize undo history)
- [vim-fugitive](https://github.com/tpope/vim-fugitive) (Git wrapper)
- [vim-gitgutter](https://github.com/airblade/vim-gitgutter) (shows git diff in the gutter)
- [nvterm](https://github.com/NvChad/nvterm) (embedded terminals)
- [neoformat](https://github.com/sbdchd/neoformat) (for code formatting. it uses a variety of formatters for many filetypes)
- [which-key](https://github.com/folke/which-key.nvim) (shows keymaps in a bottom panel)
- [lsp-zero](https://github.com/VonHeikemen/lsp-zero.nvim) (an plugin to have [nvim-cmp](https://github.com/hrsh7th/nvim-cmp) and [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig) working together)
- [Mason](https://github.com/williamboman/mason.nvim) (a plugin to install LSPs, DAPs, linters and formatters)
- [nvim-lightbulb](https://github.com/kosayoda/nvim-lightbulb) (shows a lightbulb in the gutter when a code action is available
- [cosmic-ui](https://github.com/CosmicNvim/cosmic-ui) (a plugin to rename popups and show code actions in a floating window)
- [vim-illuminate](https://github.com/RRethy/vim-illuminate) (highlights the word under the cursor matched by either LSP, Tree-sitter, or regex)
- And maybe some more that I forgot to mention. This config changes from time to time. I'll try to keep this list up to date.

## Requirements

- [Hack Nerd Font](https://www.nerdfonts.com/font-downloads) (scroll down to find the Hack font. it's for the icons)
- [kitty](https://sw.kovidgoyal.net/kitty/) (a fast GPU based terminal emulator)

## Installation

### Automatic

If you want to take the automatic route, run this:

```bash
curl -Lks https://gist.githubusercontent.com/nikolovlazar/195f33efd2cd20bb99bdc6263076cca4/raw/f2e79fed00584dbce9d577641390740f2d13227e/dotfiles-install.sh | /bin/bash
```

> This will download [this gist](https://gist.github.com/nikolovlazar/195f33efd2cd20bb99bdc6263076cca4) and pass it to `bash` for execution.

This will:

- Clone the [packer.nvim](https://github.com/wbthomason/packer.nvim) repository to the `$HOME/.local/share/nvim/site/pack/packer/start/packer.nvim` directory
- Clone this repo to the `$HOME/.dotfiles` directory
- Create a `dotfiles` alias
- Create a `.config-backup` directory
- Move any existing config into the backup directory
- Checkout the actual content from the bare repository to your `$HOME`
- Set the `showUntrackedFiles` property to `no`

### Manual

If you wish to do this manually, first clone this repo:

```bash
git clone --bare http://github.com/nikolovlazar/dotfiles.git $HOME/.dotfiles
```

Also clone the `packer.nvim` repo:

```bash
git clone --depth 1 https://github.com/wbthomason/packer.nvim ~/.local/share/nvim/site/pack/packer/start/packer.nvim
```

Define the `dotfiles` alias in the current shell scope:

```bash
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
```

Checkout the actual content from the bare repository to your `$HOME`:

```bash
dotfiles checkout
```

> You might get a warning of untracked files that will be overwritten. These are the default config files that are not in the repo. You can either delete them or back them up somewhere else.

Set the `showUntrackedFiles` flag to `no` on this specific (local) repository:

```bash
dotfiles config --local status.showUntrackedFiles no
```

You're done! You can now use the `dotfiles` alias to manage your dotfiles:

```bash
dotfiles status
dotfiles add .vimrc
dotfiles commit -m "Add vimrc"
dotfiles push
```
