---@type ChadrcConfig
--
local M = {}

M.ui = { theme = 'onedark' }

M.plugins = "custom.plugins"

M.mappings = require "custom.mappings"

vim.g.maplocalleader = ","

return M
