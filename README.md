# Lazar's dotfiles

This repository contains my dotfiles. Feel free to use them as you wish.

## Installation

### Automatic

If you want to take the automatic route, run this:

```bash
curl -Lks https://gist.githubusercontent.com/nikolovlazar/195f33efd2cd20bb99bdc6263076cca4/raw/f2e79fed00584dbce9d577641390740f2d13227e/dotfiles-install.sh | /bin/bash
```

> This will download [this gist](https://gist.github.com/nikolovlazar/195f33efd2cd20bb99bdc6263076cca4) and pass it to `bash` for execution.

This will clone this repo to the `$HOME/.dotfiles` directory, create a `dotfiles` alias, create a `.config-backup` directory, move any existing config into the backup directory, checkout the actual content from the bare repository to your `$HOME`, and set the `showUntrackedFiles` property to `no`.

### Manual

If you wish to do this manually, first clone this repo:

```bash
git clone --bare http://github.com/nikolovlazar/dotfiles.git $HOME/.dotfiles
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
