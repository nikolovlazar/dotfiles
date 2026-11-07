-- Rex keybindings
-- Navigate between panes: Ctrl+Cmd+H (left), J (down), K (up), L (right)
rex.bind("ctrl+cmd+h", "pane.focus", { direction = "left" })
rex.bind("ctrl+cmd+j", "pane.focus", { direction = "down" })
rex.bind("ctrl+cmd+k", "pane.focus", { direction = "up" })
rex.bind("ctrl+cmd+l", "pane.focus", { direction = "right" })
