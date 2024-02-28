local M = {}

M.general = {
  n = {
    ["<leader>gg"] = {"<cmd> LazyGit <CR>", "LazyGit"},
    ["<leader><leader>"] = {"<cmd> Lazy <CR>", "LazyVim"},
    ["[d"] = {vim.diagnostic.goto_prev, "Go to prev error"},
    ["]d"] = {vim.diagnostic.goto_next, "Go to next error"},
  }
}

M.conjure = {
  n = {
    ["<LocalLeader>ee"] = {"<cmd> ConjureEval <CR>", "Eval at point"},
    ["<LocalLeader>eb"] = {"<cmd> ConjureEvalBuffer <CR>", "Eval buffer"},
  }
}

M.lsp = {
  n = {
    ['gD'] = {vim.lsp.buf.declaration},
    ['gd'] = {vim.lsp.buf.definition},
    ['K'] = {vim.lsp.buf.hover},
    ['gi'] = {vim.lsp.buf.implementation},
    ['<C-k>'] = {vim.lsp.buf.signature_help},
    ['<space>wa'] = {vim.lsp.buf.add_workspace_folder},
    ['<space>wr'] = {vim.lsp.buf.remove_workspace_folder},
    ['<space>D'] = {vim.lsp.buf.type_definition},
    ['<space>rn'] = {vim.lsp.buf.rename},
    ['gr'] = {vim.lsp.buf.references},
  }
}

return M
