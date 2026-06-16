#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Resize Window to Aspect Ratio
# @raycast.mode silent
#
# Optional parameters:
# @raycast.icon 🖥️
# @raycast.author nikolovlazar
# @raycast.authorURL https://raycast.com/nikolovlazar
# @raycast.description Resizes the focused window to the largest box of the chosen aspect ratio that fits its screen, centered.
# @raycast.argument1 { "type": "dropdown", "placeholder": "Aspect Ratio", "data": [{"title": "16:9 (Landscape)", "value": "16:9"}, {"title": "4:3 (Landscape)", "value": "4:3"}, {"title": "1:1 (Square)", "value": "1:1"}, {"title": "9:16 (Portrait)", "value": "9:16"}, {"title": "3:4 (Portrait)", "value": "3:4"}] }

export RAYCAST_RATIO="${1:-16:9}"

osascript -l JavaScript <<'JXA'
ObjC.import('AppKit');

function run() {
  // --- Parse the requested ratio (e.g. "16:9") from the environment ---
  // $.getenv isn't exposed via the JXA bridge, so read it through NSProcessInfo.
  const envVal = $.NSProcessInfo.processInfo.environment.objectForKey('RAYCAST_RATIO');
  const raw = envVal.isNil() ? '16:9' : ObjC.unwrap(envVal);
  const parts = String(raw).split(':');
  const ratioW = parseFloat(parts[0]) || 16;
  const ratioH = parseFloat(parts[1]) || 9;

  // --- Grab the focused window via the Accessibility API ---
  const se = Application('System Events');
  const proc = se.applicationProcesses.whose({ frontmost: true })[0];
  const win = proc.windows[0];
  const pos = win.position(); // [x, y] in top-left-origin global coords
  const size = win.size();    // [w, h]

  // --- Figure out which screen the window lives on ---
  // NSScreen uses a bottom-left origin, so we flip Y against the primary
  // screen's full height before comparing.
  const screens = $.NSScreen.screens;
  const primaryHeight = screens.objectAtIndex(0).frame.size.height;

  const centerX = pos[0] + size[0] / 2;
  const centerYTop = pos[1] + size[1] / 2;
  const centerYBottom = primaryHeight - centerYTop;

  let target = screens.objectAtIndex(0);
  for (let i = 0; i < screens.count; i++) {
    const f = screens.objectAtIndex(i).frame;
    if (
      centerX >= f.origin.x && centerX < f.origin.x + f.size.width &&
      centerYBottom >= f.origin.y && centerYBottom < f.origin.y + f.size.height
    ) {
      target = screens.objectAtIndex(i);
      break;
    }
  }

  // visibleFrame excludes the menu bar and Dock.
  const vf = target.visibleFrame;

  // --- Largest ratioW:ratioH box that fits the visible frame ---
  // Assume full width first; if the derived height overflows, the screen is
  // "taller" than the target ratio, so clamp height and derive width instead.
  let w = vf.size.width;
  let h = (w * ratioH) / ratioW;
  if (h > vf.size.height) {
    h = vf.size.height;
    w = (h * ratioW) / ratioH;
  }

  // Center it inside the visible frame (still bottom-left origin).
  const xBottom = vf.origin.x + (vf.size.width - w) / 2;
  const yBottom = vf.origin.y + (vf.size.height - h) / 2;

  // Convert the chosen rect back to top-left origin for System Events.
  const xTop = xBottom;
  const yTop = primaryHeight - (yBottom + h);

  win.position = [Math.round(xTop), Math.round(yTop)];
  win.size = [Math.round(w), Math.round(h)];
}
JXA
