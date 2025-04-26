local route_info_provider = {}

function route_info_provider:boot(app)
  local route_info_view = {}

  function route_info_view:get(route, method)
    local line_content = vim.fn.getline(method.pos + 1)
    local indent = string.match(line_content, '^%s*')

    vim.api.nvim_set_hl(0, 'RouteInfoMethod', { fg = '#7aa2f7', italic = true })
    vim.api.nvim_set_hl(0, 'RouteInfoPath', { fg = '#9ece6a', italic = true })
    vim.api.nvim_set_hl(
      0,
      'RouteInfoMiddleware',
      { fg = '#bb9af7', italic = true }
    )

    return {
      virt_lines = {
        {
          { indent .. '  ', '@comment' },
          { table.concat(route.methods, '|'), 'RouteInfoMethod' },
          { ' /', 'RouteInfoPath' },
          { route.uri, 'RouteInfoPath' },
          { ', Middleware: [', 'comment' },
          {
            table.concat(route.middlewares or { 'None' }, ', '),
            'RouteInfoMiddleware',
          },
          { ']', 'comment' },
        },
      },
      virt_lines_above = true,
    }
  end

  app:instance('route_info_view', route_info_view)
end

return route_info_provider
