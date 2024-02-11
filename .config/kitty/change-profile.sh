#!/bin/bash

echo "Select a profile:"
echo "   (1) Coding"
echo "   (2) Screencasting"
read profile

if [ $profile -eq 1 ]; then
	rm ~/.config/kitty/kitty.conf
	cp ~/.config/kitty/kitty-coding.conf ~/.config/kitty/kitty.conf
	echo "Get in the flow!"
elif [ $profile -eq 2 ]; then
	rm ~/.config/kitty/kitty.conf
	cp ~/.config/kitty/kitty-screencasting.conf ~/.config/kitty/kitty.conf
	echo "Have fun recording!"
else
	echo "What the hell?"
fi
