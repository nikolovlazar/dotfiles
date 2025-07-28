return {
  {
    'neovim/nvim-lspconfig',
    opts = {
      servers = {
        html = {
          filetypes = { 'html', 'blade' },
          settings = {
            html = {
              format = {
                templating = true,
                wrapLineLength = 120,
                wrapAttributes = 'auto',
              },
              hover = {
                documentation = true,
                references = true,
              },
            },
          },
        },
      },
    },
  },
}