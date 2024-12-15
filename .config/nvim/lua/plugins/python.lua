return {
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "mfussenegger/nvim-dap-python",
      config = function()
        if vim.fn.has("win32") == 1 then
          require("dap-python").setup(LazyVim.get_pkg_path("debugpy", "/venv/Scripts/pythonw.exe"))
        else
          require("dap-python").setup(LazyVim.get_pkg_path("debugpy", "/venv/bin/python"))
        end

        local dap = require("dap")
        dap.configurations = dap.configurations or {}
        dap.configurations.python = dap.configurations.python or {}

        table.insert(dap.configurations.python, {
          type = "python",
          request = "launch",
          name = "Launch Django",
          program = "${workspaceFolder}/manage.py", -- Path to manage.py
          args = { "runserver", "--noreload" }, -- Prevent autoreloader issues
          django = true,
          justMyCode = false, -- Debug all code
        })

        -- Optionally, add an "Attach" configuration for attaching to a running server
        table.insert(dap.configurations.python, {
          type = "python",
          request = "attach",
          name = "Attach to Django",
          host = "127.0.0.1",
          port = 5678, -- Ensure this matches the debugpy listener port
        })
      end,
    },
  },
  {
    "linux-cultist/venv-selector.nvim",
    dependencies = { "neovim/nvim-lspconfig", "nvim-telescope/telescope.nvim", "mfussenegger/nvim-dap-python" },
    opts = {},
    keys = {
      -- Keymap to open VenvSelector to pick a venv.
      { "<leader>vs", "<cmd>VenvSelect<cr>" },
      -- Keymap to retrieve the venv from a cache (the one previously used for the same project directory).
      { "<leader>vc", "<cmd>VenvSelectCached<cr>" },
    },
  },
}
