" Specify a directory for plugins.
call plug#begin(stdpath('data') . '/plugged')

" General dependencies
Plug 'liuchengxu/vim-better-default'

Plug 'easymotion/vim-easymotion'

Plug 'axelf4/vim-strip-trailing-whitespace'
Plug 'luochen1990/rainbow'

Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-treesitter/nvim-treesitter'

" Frontend to ripgrep
Plug 'mileszs/ack.vim'

" Fuzzy file finder
" Plug 'ctrlpvim/ctrlp.vim'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.0' }

" Tree view
Plug 'preservim/nerdtree'

" Autocompletion
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
" Plug 'ncm2/float-preview.nvim'

" Async Lint Engine
Plug 'w0rp/ale'

" s-exp structural editing
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'

" vim-iced, plugin for Clojure
" Plug 'liquidz/vim-iced', {'for': 'clojure'}
" Plug 'liquidz/vim-iced-coc-source', {'for': 'clojure'}

" Conjure, plugin for Clojure/ClojureScript/Fennel/Janet
Plug 'Olical/conjure'

" Language Server Protocol
Plug 'prabirshrestha/async.vim'
Plug 'natebosch/vim-lsc'

" FZF integration
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
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

" Test runner
Plug 'antoinemadec/FixCursorHold.nvim'
Plug 'nvim-neotest/neotest'
Plug 'nvim-neotest/neotest-python'

" Terminal
Plug 'kassio/neoterm'

" Fennel (Lisp for LUA)
Plug 'bakpakin/fennel.vim'

" Python black
Plug 'psf/black', { 'branch': 'stable' }

" org-mode for neo-vim
" Plug 'nvim-treesitter/nvim-treesitter'
" Plug 'nvim-orgmode/orgmode'

" DAP mode (debugging)
Plug 'mfussenegger/nvim-dap'
Plug 'mfussenegger/nvim-dap-python'

" Better motions
Plug 'easymotion/vim-easymotion'

" Themes
Plug 'NLKNguyen/papercolor-theme'
Plug 'arcticicestudio/nord-vim'
Plug 'morhetz/gruvbox'
Plug 'romainl/Apprentice'
Plug 'pineapplegiant/spaceduck'

" status line setup (trying airline for now
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Nice icons
Plug 'ryanoasis/vim-devicons'

" Initialize plugin system.
let g:python3_host_prog = '/home/foca/.venvs/neovim/bin/python3'

call plug#end()

" colorscheme PaperColor
" colorscheme gruvbox
" colorscheme apprentice
set termguicolors
set background=dark
colorscheme PaperColor

" Activate rainbow parens
let g:rainbow_active = 1

" ctrlp customization
let g:ctrlp_root_markers = ['deps.edn']
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" ack.vim customization to use ripgrep
let g:ackprg = 'rg --vimgrep'
let g:ack_autoclose = 1
cnoreabbrev Ack Ack!

" let g:deoplete#enable_at_startup = 1
" call deoplete#custom#option('keyword_patterns', {'clojure': '[\w!$%&*+/:<=>?@\^_~\-\.#]*'})
" set completeopt-=preview

let g:float_preview#docked = 0
let g:float_preview#max_width = 80
let g:float_preview#max_height = 40

" c/c++ language server setup with ccls, start -->
" Register ccls C++ lanuage server.
let g:lsc_server_commands = {
\ 'cpp': {
\    'command': 'ccls',
\    'message_hooks': {
\        'initialize': {
\            'initializationOptions': {'cache': {'directory': '/tmp/ccls/cache'}},
\            'rootUri': {m, p -> lsc#uri#documentUri(fnamemodify(findfile('compile_commands.json', expand('%:p') . ';'), ':p:h'))}
\        },
\    },
\  },
\}

let g:lsc_auto_map = {
\  'GoToDefinition': '<M-d>',
\  'FindReferences': '<M-r>',
\}
" <-- end, c/c++ language server setup with ccls

" For ALE, linters for clojure, cpp, elixir
let g:ale_linters = {
      \ 'clojure': ['clj-kondo'],
      \ 'cpp': ['ccls'],
      \ 'elixir': ['mix_format']
      \}

" Language server for python
if executable('pyls')
    " pip install python-language-server
    au User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ })
endif

" COC related settings
" see here for more:
" https://github.com/neoclide/coc.nvim/blob/master/doc/coc.txt#L912
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gr <Plug>(coc-references)
" shows code outline
nnoremap <silent> <leader>co  :<C-u>CocList outline<CR>

" Test runner settings
let test#strategy = "neoterm"

" Enable per project settings
set exrc
set secure

" run black infile save
autocmd BufWritePre *.py execute ':Black'
let maplocalleader = ","
let mapleader = ","

" Tell vim-iced to use <LocalLeader>
let g:iced_enable_default_key_mappings = v:true

" Bindings for NERDTree
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
" nnoremap <C-f> :NERDTreeFind<CR>

" [DISABLED] map CTRL-p to FZF instead of ctrlp plugin
" nnoremap <C-p> :GFiles<Cr>

" Find files using Telescope command-line sugar.
nnoremap <C-p> <cmd>Telescope find_files<cr>
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>
