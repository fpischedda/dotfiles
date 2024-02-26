local M = {}

M.general = {
  n = {
    ["<leader>gg"] = {
      function()
        local term = require("nvterm.terminal").new("float")
        vim.api.nvim_chan_send(term.job_id, "lazygit\n")
      end,
      "open Lazygit",
    },
  }
}

M.conjure = {
  n = {
    ["<LocalLeader>ee"] = {"<cmd> ConjureEval <CR>", "Eval at point"},
    ["<LocalLeader>eb"] = {"<cmd> ConjureEvalBuffer <CR>", "Eval buffer"},
  }
}

return M
