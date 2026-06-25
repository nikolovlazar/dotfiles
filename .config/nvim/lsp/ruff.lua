-- Ruff (Python linter/formatter LSP). Hover is disabled so pyright owns it.
return {
  cmd_env = { RUFF_TRACE = 'messages' },
  init_options = {
    settings = {
      logLevel = 'error',
    },
  },
  on_attach = function(client)
    client.server_capabilities.hoverProvider = false
  end,
}
