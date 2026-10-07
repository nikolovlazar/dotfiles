# Lazar's dotfiles ✨💽

This repository contains my ever-evolving dotfiles. Check them out! If you find something useful, feel free to add it to your own dotfiles.

## Neovim Plugins

- [Kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim) (base configuration)
- [Catppuccin](https://github.com/catppuccin/nvim) (colorscheme)
- [Tokyonight Tmux](https://github.com/nikolovlazar/tokyo-night-tmux) (tmux theme integration)
- [aserowy/tmux.nvim](https://github.com/aserowy/tmux.nvim) (tmux + neovim integration)

## Requirements

- [A Nerd Font](https://www.nerdfonts.com/font-downloads) (it's for the icons)
- [Wezterm](https://wezfurlong.org/wezterm/) (a powerful cross-platform terminal emulator and multiplexer written in Rust)

## Setup with GNU Stow

Install GNU Stow and place the checkout at `~/dotfiles`. The checkout is one Stow package, named `.`. Home directories remain real directories; Stow creates symlinks for the approved configuration files inside them.

First inspect the proposed links:

```sh
stow --simulate --verbose --no-folding --dir="$HOME/dotfiles" --target="$HOME" .
```

Once the dry run reports no conflicts, apply the same command without `--simulate`:

```sh
stow --verbose --no-folding --dir="$HOME/dotfiles" --target="$HOME" .
```

The explicit `--no-folding` flag makes these commands safe to invoke from any working directory. The repository's `.stowrc` also sets it for commands run inside the checkout. Stow reads `.stowrc` from the working directory and home directory, so specifying `--dir` alone does not load the repository's `.stowrc`.

`--no-folding` does not convert existing directory symlinks into real directories. Before migrating an older setup, back up the full checkout, including ignored and untracked files, and record the existing links. Close applications that write to the affected directories. Replace broad directory links with real home directories, keep local state in those directories, and retain only the approved portable configuration in the checkout. Preserve permissions and repair relative links whose resolved destinations change when files move. Then run the dry run above.

Do not use `--adopt` to resolve conflicts: it moves home files into the checkout and can import private or generated state. Compare each conflicting file and preserve a private backup before choosing which version belongs in the repository.

## What this repository tracks

Git and Stow have separate allowlists. `.gitignore` controls which new files normal Git staging includes; `.stow-local-ignore` controls what can be linked into home. Both must be updated when deliberately adding a new application.

The approved `.config` directories are `btop`, `ghostty`, `kitty`, `lazydocker`, `lazygit`, `mise`, `nvim`, `tmux`, and `wezterm`. Raycast is limited to `.config/raycast/scripts`; Zed is limited to `.config/zed/settings.json`.

The other approved configuration is:

- `.agents/skills` and `.agents/.skill-lock.json`, excluding skill-studio quarantine and generated agent state.
- `.claude/settings.json` only. Local settings, hooks, plugins, skills, credentials, and session state remain local.
- `.emacs.d` source configuration, excluding private `local.el`, packages, databases, caches, and editor state.
- `.oh-my-zsh/themes`.
- `Library/Application Support/Cursor/User/settings.json` and `keybindings.json` only.
- `.zshrc`.

Repository management files, including this README and the Git/Stow policies, remain in the checkout and are not linked into home. Existing editor exclusions continue to apply, while Neovim's `lazy-lock.json` remains tracked. Config logs, installed tmux plugins, lazygit state, and editor backup/lock files remain local. Installing an unrelated application or creating a new cache directory under home does not add files to the repository.

## Deliberately adding another application

1. Install and configure the application normally. Its files stay under home until you deliberately choose to manage them here.
2. Choose its portable configuration files. Keep credentials, local machine settings, caches, histories, and generated files in home.
3. Move only the chosen files into the matching path in the checkout. For example, for `new-app/config.toml`:

   ```sh
   mkdir -p "$HOME/dotfiles/.config/new-app"
   mv "$HOME/.config/new-app/config.toml" "$HOME/dotfiles/.config/new-app/config.toml"
   ```

4. Add the chosen path to both `.gitignore` and `.stow-local-ignore`, including exclusions for its local state. Insert Git allow rules in the selected application section, before the final generated-file exclusions; appending allow rules at the end can reopen logs and backups. Stow uses Perl regular expressions; Git's `!` negation syntax does not apply to Stow.
5. Run the Stow dry run, then apply the command above. Confirm the application still reads its config and that new local state stays under home.
6. Review and explicitly stage only the intended files:

   ```sh
   git -C "$HOME/dotfiles" status --short
   git -C "$HOME/dotfiles" diff -- .gitignore .stow-local-ignore .config/new-app/config.toml
   git -C "$HOME/dotfiles" add .gitignore .stow-local-ignore .config/new-app/config.toml
   git -C "$HOME/dotfiles" diff --cached
   ```

Git ignore rules do not remove files that are already tracked. When tightening the policy, stop tracking excluded paths with `git rm --cached` after preserving their local copies and verifying the proposed removals.

## When an application replaces a config symlink

Some applications save settings by replacing the file rather than writing through its symlink. If a home settings path becomes a regular file, Stow's dry run reports a conflict. For a settings-only example, inspect whether the link still exists and compare the two versions:

```sh
test -L "$HOME/.config/zed/settings.json"
diff -u "$HOME/dotfiles/.config/zed/settings.json" "$HOME/.config/zed/settings.json"
```

Preserve a private backup of the home version. If its edits should be tracked, copy the reviewed changes into the repository version. Move the conflicting home file to that backup location, then rerun the Stow dry run and apply the links. This makes the retained version explicit and restores the link without importing unrelated local files.
