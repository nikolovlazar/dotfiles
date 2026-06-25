-- Completion via blink.cmp — a single fast (Rust core) plugin that replaces
-- nvim-cmp + its source plugins + LuaSnip. `version = '*'` pulls a prebuilt
-- binary, so no local Rust toolchain is required.
return {
  {
    'saghen/blink.cmp',
    event = 'InsertEnter',
    version = '*',
    dependencies = { 'folke/lazydev.nvim' },
    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      -- Default preset: <C-y> accept, <C-n>/<C-p> select, <C-space> open/docs,
      -- <C-e> hide, <C-b>/<C-f> scroll docs, <Tab>/<S-Tab> snippet jump.
      keymap = { preset = 'default' },
      appearance = { nerd_font_variant = 'mono' },
      completion = {
        documentation = { auto_show = true, auto_show_delay_ms = 200 },
      },
      sources = {
        default = { 'lsp', 'path', 'snippets', 'lazydev' },
        providers = {
          -- lazydev completions for editing this Neovim config; high score so
          -- they outrank (and dedupe) lua_ls's own suggestions.
          lazydev = {
            name = 'LazyDev',
            module = 'lazydev.integrations.blink',
            score_offset = 100,
          },
        },
      },
      -- Use blink's built-in snippet engine (no LuaSnip).
      snippets = { preset = 'default' },
      fuzzy = { implementation = 'prefer_rust_with_warning' },
    },
  },
}
-- vim: ts=2 sts=2 sw=2 et
