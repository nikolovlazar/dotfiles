-- Astro config
vim.g.astro_typescript = true
vim.g.astro_stylus = true

vim.notify = require("notify")

vim.cmd([[
let g:copilot_filetypes = { 'markdown': v:true, }
let g:sneak#label = 1
let g:netrw_banner = 0
]])
