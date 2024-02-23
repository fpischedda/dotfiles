local M = {}

M.conjure = {
  n = {
    ["<LocalLeader>ee"] = {"<cmd> ConjureEval <CR>", "Eval at point"},
    ["<LocalLeader>eb"] = {"<cmd> ConjureEvalBuffer <CR>", "Eval buffer"},
  },
  i = {
    ["<C-a>ee"] = {"<cmd> ConjureEval <CR>", "Eval at point"},
    ["<C-a>eb"] = {"<cmd> ConjureEvalBuffer <CR>", "Eval buffer"},
  }
}

return M
