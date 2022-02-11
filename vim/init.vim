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
" Plug 'ctrlpvim/ctrlp.vim'

" Tree view
Plug 'preservim/nerdtree'

" Autocompletion
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
" Plug 'ncm2/float-preview.nvim'

" Async Lint Engine
Plug 'w0rp/ale'

" Finding files
" Plug 'liuchengxu/vim-clap'

" vim-iced, plugin for Clojure
Plug 'guns/vim-sexp'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'liquidz/vim-iced', {'for': 'clojure'}
Plug 'liquidz/vim-iced-coc-source', {'for': 'clojure'}

" Conjure, plugin for Clojure/ClojureScript/Fennel/Janet
" Plug 'Olical/conjure', { 'tag': 'v4.2.0' }
" Plug 'tpope/vim-dispatch'
" Plug 'clojure-vim/vim-jack-in'
" Plug 'radenling/vim-dispatch-neovim'

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
Plug 'vim-test/vim-test'

" Terminal
Plug 'kassio/neoterm'

" Fennel (Lisp for LUA)
Plug 'bakpakin/fennel.vim'

" Python black
Plug 'psf/black', { 'branch': 'stable' }

" Themes
Plug 'NLKNguyen/papercolor-theme'
Plug 'arcticicestudio/nord-vim'
Plug 'morhetz/gruvbox'
Plug 'romainl/Apprentice'

" Initialize plugin system.
call plug#end()

" colorscheme PaperColor
" colorscheme gruvbox
" colorscheme apprentice
set termguicolors
set background=dark
colorscheme toast

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
