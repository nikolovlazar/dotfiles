# Lazar's dotfiles

This repository contains my dotfiles. Feel free to use them as you wish.

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
