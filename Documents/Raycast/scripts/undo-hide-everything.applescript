#!/usr/bin/osascript

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Undo Recording Mode
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🎥

# Documentation:
# @raycast.description Undo the "Recording Mode" script
# @raycast.author nikolovlazar
# @raycast.authorURL https://raycast.com/nikolovlazar

tell application "System Events"
	-- Hide the menu bar
	tell dock preferences to set autohide menu bar to false
	-- Hide the icons
	do shell script "defaults write com.apple.finder CreateDesktop -bool TRUE; killall Finder"
	-- Don't let the dock pop up when you mouse down
	do shell script "defaults delete com.apple.dock autohide-delay; killall Dock"
	-- Ensure the dock remains hidden but can be activated on hover
	tell dock preferences to set autohide to true
end tell

