#!/bin/bash

echo "Select a profile:"
echo "   (1) Coding"
echo "   (2) Screencasting"
read -r profile

if [ "$profile" -eq 1 ]; then
	rm ~/dotfiles/.config/kitty/kitty.conf
	cp ~/dotfiles/.config/kitty/kitty-coding.conf ~/dotfiles/.config/kitty/kitty.conf
	echo "Get in the flow!"
elif [ "$profile" -eq 2 ]; then
	rm ~/dotfiles/.config/kitty/kitty.conf
	cp ~/dotfiles/.config/kitty/kitty-screencasting.conf ~/dotfiles/.config/kitty/kitty.conf
	echo "Have fun recording!"
else
	echo "What the hell?"
fi
