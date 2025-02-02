#!/usr/bin/osascript

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Screencast Window
# @raycast.mode compact

# Optional parameters:
# @raycast.icon 🪟

# Documentation:
# @raycast.description Position currently focused window for screencasting
# @raycast.author nikolovlazar
# @raycast.authorURL https://raycast.com/nikolovlazar

-- The resolution of your screen
set screenWidth to 1920
set screenHeight to 1080

-- Padding
set gutter to 12
-- Bottom padding
set bottomGutter to 35

tell application "System Events"
	-- Position currently focused window for recording
	set appOfInterest to name of application processes whose frontmost is true
	
	set currentApplication to item 1 of appOfInterest
	
	-- Top left corner
	set position of the first window of application process currentApplication to {gutter, gutter}
	-- Size of the window
	set size of the first window of application process currentApplication to {screenWidth - gutter - gutter, screenHeight - gutter - bottomGutter}
end tell

