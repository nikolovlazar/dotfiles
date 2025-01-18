local M = {}

M.bg_blurred_darker = os.getenv 'HOME'
  .. '/dotfiles/.config/wezterm/assets/bg-blurred-darker.png'
M.bg_blurred = os.getenv 'HOME'
  .. '/dotfiles/.config/wezterm/assets/bg-blurred.png'
M.bg_image = M.bg_blurred_darker

return M
