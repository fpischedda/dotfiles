" Specify a directory for plugins.
call plug#begin(stdpath('data') . '/plugged')

" Specify your required plugins here.
Plug 'liuchengxu/vim-better-default'

Plug 'easymotion/vim-easymotion'

Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'

" Autocompletion
Plug 'Shougo/deoplete.nvim'
Plug 'ncm2/float-preview.nvim'

" Async Lint Engine
Plug 'w0rp/ale'

" Finding files
Plug 'liuchengxu/vim-clap'

" Conjure, plugin for Clojure/ClojureScript
Plug 'Olical/conjure', { 'tag': 'v2.1.0', 'do': 'bin/compile' }

" Language Server Protocol
Plug 'prabirshrestha/async.vim'
Plug 'prabirshrestha/vim-lsp'

" FZF integration
Plug '/usr/fzf'
Plug 'junegunn/fzf.vim'

" Markdown syntax highlight
Plug 'tpope/vim-markdown'

" Show changed lines
Plug 'airblade/vim-gitgutter'

" Git wrapper
Plug 'tpope/vim-fugitive'

" Better repeat handling, especially for plugins
Plug 'tpope/vim-repeat'

Plug 'tpope/vim-surround'

" Modern c++ syntax highlight
Plug 'bfrg/vim-cpp-modern'

" CMake support
Plug 'vhdirk/vim-cmake'

" Initialize plugin system.
call plug#end()

let g:deoplete#enable_at_startup = 1
call deoplete#custom#option('keyword_patterns', {'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*'})
set completeopt-=preview

let g:float_preview#docked = 0
let g:float_preview#max_width = 80
let g:float_preview#max_height = 40

" For ALE, linters for clojure
let g:ale_linters = {
      \ 'clojure': ['clj-kondo', 'joker']
      \}

" Vim Clap
let g:clap_provider_grep_delay = 50
let g:clap_provider_grep_opts = '-H --no-heading --vimgrep --smart-case --hidden -g "!.git/"'

nnoremap <leader>* :Clap grep ++query=<cword><cr>
nnoremap <leader>fg :Clap grep<cr>
nnoremap <leader>ff :Clap files --hidden<cr>
nnoremap <leader>fb :Clap buffers<cr>
nnoremap <leader>fw :Clap windows<cr>
nnoremap <leader>fr :Clap history<cr>
nnoremap <leader>fh :Clap command_history<cr>
nnoremap <leader>fj :Clap jumps<cr>
nnoremap <leader>fl :Clap blines<cr>
nnoremap <leader>fL :Clap lines<cr>
nnoremap <leader>ft :Clap filetypes<cr>
nnoremap <leader>fm :Clap marks<cr>

" Language server for python
if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

" Enable per project settings
set exrc
set secure
