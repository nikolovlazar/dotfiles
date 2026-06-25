-- autopairs
-- https://github.com/windwp/nvim-autopairs

return {
  'windwp/nvim-autopairs',
  event = 'InsertEnter',
  -- Bracket/quote pairing while typing. Completion-driven bracket insertion
  -- (adding `()` after accepting a function) is handled by blink.cmp itself.
  opts = {},
}
