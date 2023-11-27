-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

local opt = vim.opt

opt.conceallevel = 0

-- https://github.com/neovim/neovim/issues/21749#issuecomment-1378720864
-- Fix loading of json5
package.cpath = package.cpath .. "/?.dylib"
table.insert(vim._so_trails, "/?.dylib")
