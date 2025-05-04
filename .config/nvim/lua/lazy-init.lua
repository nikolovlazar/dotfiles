local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  -- bootstrap lazy.nvim
  -- stylua: ignore
  vim.fn.system({ "git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable",
    lazypath })
end
vim.opt.rtp:prepend(vim.env.LAZY or lazypath)

require('lazy').setup({
  spec = {
    'tpope/vim-sleuth',
    { import = 'plugins.coding.ai' },
    { import = 'plugins.coding.autopairs' },
    { import = 'plugins.coding.cmp' },
    { import = 'plugins.coding.lspconfig' },
    { import = 'plugins.coding.todo-comments' },
    { import = 'plugins.coding.treesitter' },
    { import = 'plugins.coding.trouble' },
    { import = 'plugins.dap.core' },
    { import = 'plugins.editor.fzf' },
    { import = 'plugins.editor.gitsigns' },
    { import = 'plugins.editor.grug-far' },
    { import = 'plugins.editor.lazygit' },
    { import = 'plugins.editor.mini' },
    { import = 'plugins.editor.file-tree' },
    { import = 'plugins.editor.overseer' },
    { import = 'plugins.editor.tmux' },
    { import = 'plugins.editor.ufo' },
    { import = 'plugins.editor.which-key' },
    { import = 'plugins.formatting.conform' },
    { import = 'plugins.formatting.prettier' },
    { import = 'plugins.languages.astro' },
    { import = 'plugins.languages.docker' },
    { import = 'plugins.languages.go' },
    { import = 'plugins.languages.laravel' },
    { import = 'plugins.languages.mdx' },
    { import = 'plugins.languages.php' },
    { import = 'plugins.languages.python' },
    { import = 'plugins.languages.tailwind' },
    { import = 'plugins.languages.typescript' },
    { import = 'plugins.linting.core' },
    { import = 'plugins.test.core' },
    { import = 'plugins.ui.colorscheme' },
    { import = 'plugins.ui.dressing' },
    { import = 'plugins.ui.treesitter-context' },
    { import = 'plugins.util.mini-hipatterns' },
  },
  defaults = {},
  performance = {
    rtp = {
      disabled_plugins = {
        'gzip',
        'tarPlugin',
        'zipPlugin',
        'netrwPlugin',
        'matchit',
        'matchparen',
        'shada',
        'spellfile',
      },
    },
  },
}, {
  ui = {
    -- If you are using a Nerd Font: set icons to an empty table which will use the
    -- default lazy.nvim defined Nerd Font icons, otherwise define a unicode icons table
    icons = vim.g.have_nerd_font and {} or {
      cmd = '⌘',
      config = '🛠',
      event = '📅',
      ft = '📂',
      init = '⚙',
      keys = '🗝',
      plugin = '🔌',
      runtime = '💻',
      require = '🌙',
      source = '📄',
      start = '🚀',
      task = '📌',
      lazy = '💤 ',
    },
  },
})
