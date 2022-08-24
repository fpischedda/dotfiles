" Specify a directory for plugins.
call plug#begin(stdpath('data') . '/plugged')

" Specify your required plugins here.

Plug 'liuchengxu/vim-better-default'

Plug 'easymotion/vim-easymotion'

Plug 'axelf4/vim-strip-trailing-whitespace'
Plug 'luochen1990/rainbow'

" Frontend to ripgrep
Plug 'mileszs/ack.vim'

" Fuzzy file finder
Plug 'ctrlpvim/ctrlp.vim'

" Tree view
Plug 'preservim/nerdtree'

" Autocompletion
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'ncm2/float-preview.nvim'


" s-exp structural editing
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'

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
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'antoinemadec/FixCursorHold.nvim'
Plug 'nvim-neotest/neotest'

" Terminal
Plug 'kassio/neoterm'

" Fennel (Lisp for LUA)
Plug 'bakpakin/fennel.vim'

" Python black
Plug 'psf/black', { 'branch': 'stable' }

" DAP mode (debugging)
Plug 'mfussenegger/nvim-dap'
Plug 'mfussenegger/nvim-dap-python'

" Themes
Plug 'NLKNguyen/papercolor-theme'
Plug 'arcticicestudio/nord-vim'
Plug 'morhetz/gruvbox'
Plug 'romainl/Apprentice'
Plug 'pineapplegiant/spaceduck'

" status line setup (trying airline for now
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Initialize plugin system.
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

" Tell vim-iced to use <LocalLeader>
let g:iced_enable_default_key_mappings = v:true

" Bindings for NERDTree
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
" nnoremap <C-f> :NERDTreeFind<CR>

" map CTRL-p to FZF instead of ctrlp plugin
nnoremap <C-p> :GFiles<Cr>

autocmd FileType python let b:coc_root_patterns = ['.git', 'env', '.env']

" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice.
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Use K to show documentation in preview window.
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction
