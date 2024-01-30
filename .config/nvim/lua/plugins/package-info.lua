return {
  {
    "vuki656/package-info.nvim",
    ft = "json",
    dependencies = { "MunifTanjim/nui.nvim" },
    config = function()
      local colors = require("tokyonight.colors").setup()

      require("package-info").setup({
        autostart = false,
        package_manager = "pnpm",
        colors = {
          outdated = colors.red1,
        },
        hide_up_to_date = true,
      })
    end,
  },
}
