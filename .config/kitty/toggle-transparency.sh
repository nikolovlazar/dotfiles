#!/bin/bash

if grep -q 'background_opacity 0.9' ~/dotfiles/.config/kitty/current-theme.conf; then
	sed -i '' 's/background_opacity 0.9/background_opacity 1/g' ~/dotfiles/.config/kitty/current-theme.conf
	echo "Done! Reload kitty config!"
elif grep -q 'background_opacity 1' ~/dotfiles/.config/kitty/current-theme.conf; then
	sed -i '' 's/background_opacity 1/background_opacity 0.9/g' ~/dotfiles/.config/kitty/current-theme.conf
	echo "Done! Reload kitty config!"
else
	echo "There's no background_opacity present in the current kitty theme..."
fi
