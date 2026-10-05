return {
  {
    'iamcco/markdown-preview.nvim',
    cmd = { 'MarkdownPreviewToggle', 'MarkdownPreview', 'MarkdownPreviewStop' },
    ft = { 'markdown' },
    build = function(plugin)
      vim.cmd.source(plugin.dir .. '/autoload/mkdp/util.vim')
      vim.fn['mkdp#util#install_sync']()
    end,
    keys = {
      {
        '<leader>cp',
        '<cmd>MarkdownPreviewToggle<cr>',
        ft = 'markdown',
        desc = 'Markdown Preview',
      },
    },
  },
}
