return {
  {
    "folke/zen-mode.nvim",
    opts = {
      window = {
        width = 80,
      },
      plugins = {
        options = {
          laststatus = 0,
        },
      },
      on_open = function()
        require("package-info").hide()
      end,
    },
  },
}
