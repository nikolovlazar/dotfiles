return {
  {
    "vuki656/package-info.nvim",
    ft = "json",
    dependencies = { "MunifTanjim/nui.nvim" },
    config = function()
      local colors = require("catppuccin.palettes").get_palette("mocha")

      require("package-info").setup({
        autostart = false,
        package_manager = "pnpm",
        colors = {
          outdated = colors.peach,
        },
        hide_up_to_date = true,
      })
    end,
  },
}
