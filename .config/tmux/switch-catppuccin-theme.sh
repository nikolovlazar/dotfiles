#!/bin/bash
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:$PATH"

echo "TMUX_SOCKET=$TMUX_SOCKET" > /tmp/tmux-debug.log
tmux -S "$TMUX_SOCKET" list-sessions >> /tmp/tmux-debug.log 2>&1 || echo "No sessions found" >> /tmp/tmux-debug.log

set -euo pipefail

FLAVOR="$1"
THEME_DIR="$HOME/.config/tmux/plugins/tmux/themes"
TMUX_SOCKET=$(tmux display-message -p -F "#{socket_path}")

tmux -S "$TMUX_SOCKET" set -g @catppuccin_flavor "$FLAVOR"
tmux -S "$TMUX_SOCKET" source-file "$THEME_DIR/catppuccin_${FLAVOR}_tmux.conf"

case "$FLAVOR" in
  latte)
    tmux -S "$TMUX_SOCKET" set -g @thm_bg "#eff1f5"
    tmux -S "$TMUX_SOCKET" set -g @thm_fg "#4c4f69"
    tmux -S "$TMUX_SOCKET" set -g @thm_rosewater "#dc8a78"
    tmux -S "$TMUX_SOCKET" set -g @thm_flamingo "#dd7878"
    tmux -S "$TMUX_SOCKET" set -g @thm_pink "#ea76cb"
    tmux -S "$TMUX_SOCKET" set -g @thm_mauve "#8839ef"
    tmux -S "$TMUX_SOCKET" set -g @thm_red "#d20f39"
    tmux -S "$TMUX_SOCKET" set -g @thm_maroon "#e64553"
    tmux -S "$TMUX_SOCKET" set -g @thm_peach "#fe640b"
    tmux -S "$TMUX_SOCKET" set -g @thm_yellow "#df8e1d"
    tmux -S "$TMUX_SOCKET" set -g @thm_green "#40a02b"
    tmux -S "$TMUX_SOCKET" set -g @thm_teal "#179299"
    tmux -S "$TMUX_SOCKET" set -g @thm_sky "#04a5e5"
    tmux -S "$TMUX_SOCKET" set -g @thm_sapphire "#209fb5"
    tmux -S "$TMUX_SOCKET" set -g @thm_blue "#1e66f5"
    tmux -S "$TMUX_SOCKET" set -g @thm_lavender "#7287fd"
    tmux -S "$TMUX_SOCKET" set -g @thm_subtext_1 "#a6adc8"
    tmux -S "$TMUX_SOCKET" set -g @thm_subtext_0 "#bac2de"
    tmux -S "$TMUX_SOCKET" set -g @thm_overlay_2 "#9399b2"
    tmux -S "$TMUX_SOCKET" set -g @thm_overlay_1 "#7f849c"
    tmux -S "$TMUX_SOCKET" set -g @thm_overlay_0 "#6c7086"
    tmux -S "$TMUX_SOCKET" set -g @thm_surface_2 "#585b70"
    tmux -S "$TMUX_SOCKET" set -g @thm_surface_1 "#45475a"
    tmux -S "$TMUX_SOCKET" set -g @thm_surface_0 "#313244"
    tmux -S "$TMUX_SOCKET" set -g @thm_mantle "#181825"
    tmux -S "$TMUX_SOCKET" set -g @thm_crust "#11111b"
    ;;
  mocha)
    tmux -S "$TMUX_SOCKET" set -g @thm_bg "#1e1e2e"
    tmux -S "$TMUX_SOCKET" set -g @thm_fg "#cdd6f4"
    tmux -S "$TMUX_SOCKET" set -g @thm_rosewater "#f5e0dc"
    tmux -S "$TMUX_SOCKET" set -g @thm_flamingo "#f2cdcd"
    tmux -S "$TMUX_SOCKET" set -g @thm_pink "#f5c2e7"
    tmux -S "$TMUX_SOCKET" set -g @thm_mauve "#cba6f7"
    tmux -S "$TMUX_SOCKET" set -g @thm_red "#f38ba8"
    tmux -S "$TMUX_SOCKET" set -g @thm_maroon "#eba0ac"
    tmux -S "$TMUX_SOCKET" set -g @thm_peach "#fab387"
    tmux -S "$TMUX_SOCKET" set -g @thm_yellow "#f9e2af"
    tmux -S "$TMUX_SOCKET" set -g @thm_green "#a6e3a1"
    tmux -S "$TMUX_SOCKET" set -g @thm_teal "#94e2d5"
    tmux -S "$TMUX_SOCKET" set -g @thm_sky "#89dceb"
    tmux -S "$TMUX_SOCKET" set -g @thm_sapphire "#74c7ec"
    tmux -S "$TMUX_SOCKET" set -g @thm_blue "#89b4fa"
    tmux -S "$TMUX_SOCKET" set -g @thm_lavender "#b4befe"
    tmux -S "$TMUX_SOCKET" set -g @thm_subtext_1 "#a6adc8"
    tmux -S "$TMUX_SOCKET" set -g @thm_subtext_0 "#bac2de"
    tmux -S "$TMUX_SOCKET" set -g @thm_overlay_2 "#9399b2"
    tmux -S "$TMUX_SOCKET" set -g @thm_overlay_1 "#7f849c"
    tmux -S "$TMUX_SOCKET" set -g @thm_overlay_0 "#6c7086"
    tmux -S "$TMUX_SOCKET" set -g @thm_surface_2 "#585b70"
    tmux -S "$TMUX_SOCKET" set -g @thm_surface_1 "#45475a"
    tmux -S "$TMUX_SOCKET" set -g @thm_surface_0 "#313244"
    tmux -S "$TMUX_SOCKET" set -g @thm_mantle "#181825"
    tmux -S "$TMUX_SOCKET" set -g @thm_crust "#11111b"
    ;;
  *)
    echo "Unknown flavor: $FLAVOR"
    exit 1
    ;;
esac

tmux -S "$TMUX_SOCKET" set -g status-style "bg=none,fg=#{@thm_fg}"
tmux -S "$TMUX_SOCKET" set -g message-style "bg=none,fg=#{@thm_fg}"
tmux -S "$TMUX_SOCKET" set -g pane-border-style "fg=#{@thm_surface_2}"
tmux -S "$TMUX_SOCKET" set -g pane-active-border-style "fg=#{@thm_blue}"

echo "✅ Switched to Catppuccin $FLAVOR"
