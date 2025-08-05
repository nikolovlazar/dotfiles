#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title New Video Project Structure
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 📂
# @raycast.author nikolovlazar
# @raycast.authorURL https://raycast.com/nikolovlazar
# @raycast.description Creates a folder structure for a new video project
# @raycast.argument1 { "type": "text", "placeholder": "slug" }

date=$(date "+%Y%m%d")
mkdir ~/Movies/${date}-${1//}
cd ~/Movies/${date}-${1//}

folders=("FOOTAGE" "AUDIO" "MUSIC" "GRAPHICS" "AE" "SEQUENCES" "EXPORTS" "PREMIERE")

for ((i = 0; i < ${#folders[@]}; i++)); do
	index=$((i+1))
	dirname=$(printf "%02d %s" "$index" "${folders[$i]}")
	mkdir "$dirname"
done
