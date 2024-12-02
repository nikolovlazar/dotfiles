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
}
