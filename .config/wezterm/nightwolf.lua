-- nightwolf theme for wezterm
-- optimized from https://github.com/mao-santaella-rs/NightWolfTheme
return {
  -- Main colors
  foreground = '#c8c8c8', -- text color (200,200,200)
  background = '#000000',

  -- Cursor colors
  cursor_bg = '#969696', -- principal_5 (150,150,150)
  cursor_fg = '#141414',
  cursor_border = '#969696',

  -- Selection colors
  selection_fg = '#141414',
  selection_bg = '#969696', -- Using principal_5 at higher opacity

  -- UI elements
  scrollbar_thumb = '#282828', -- principal_1 (40,40,40)
  split = '#3c3c3c', -- principal_2 (60,60,60)

  -- Terminal colors (ANSI)
  ansi = {
    '#141414', -- black (principal_0)
    '#ef5350', -- red (based on gitRemovedBg)
    '#99b76d', -- green (based on gitInsertedBg)
    '#ffca28', -- yellow (assumed syntaxYellow)
    '#42a5f5', -- blue (assumed syntaxBlue)
    '#7e57c2', -- purple (assumed syntaxPurple)
    '#26c6da', -- cyan (assumed syntaxCyan)
    '#c8c8c8', -- white (text)
  },

  -- Bright terminal colors
  brights = {
    '#505050', -- bright black (principal_3)
    '#ef5350', -- bright red
    '#99b76d', -- bright green
    '#ffa726', -- bright orange (assumed syntaxOrange)
    '#42a5f5', -- bright blue
    '#ab47bc', -- bright violet (assumed syntaxViolet)
    '#26c6da', -- bright cyan
    '#e0e0e0', -- bright white (brighter than text)
  },

  indexed = { [17] = '#ef5350' },
}
