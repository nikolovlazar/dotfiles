#!/bin/bash

# Count panes in the current tmux window
pane_count=$(tmux list-panes | wc -l)

if [ "$pane_count" -gt 1 ]; then
  tmux set -g pane-border-lines single
  tmux set -g pane-border-status top
  tmux set -g pane-border-format " Pane ###{pane_index} "
else
  tmux set -g pane-border-lines single
  tmux set -g pane-border-status off
  tmux set -g pane-border-format ""
fi
