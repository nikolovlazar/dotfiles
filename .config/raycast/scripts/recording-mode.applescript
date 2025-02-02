#!/usr/bin/osascript

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Recording mode
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🎥

# Documentation:
# @raycast.description Hide icons, menu bar, and dock, and change background
# @raycast.author nikolovlazar
# @raycast.authorURL https://raycast.com/nikolovlazar

tell application "System Events"
	-- Hide the menu bar
	tell dock preferences to set autohide menu bar to true
	-- Hide the dock
	tell dock preferences to set autohide to true
	-- Hide the icons
	do shell script "defaults write com.apple.finder CreateDesktop -bool FALSE; killall Finder"
	-- Don't let the dock pop up when you mouse down
	do shell script "defaults write com.apple.dock autohide-delay -float 100 && killall Dock"
end tell

