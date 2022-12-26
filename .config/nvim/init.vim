" Global Settings {{{
set number
set foldmethod=marker
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab
set mouse=a
" }}}

" Keybindings {{{
let mapleader=" "
nnoremap q :q<CR>
nnoremap <leader>w <C-w><C-w>
nnoremap <leader>e :Ex<CR>
nnoremap <leader>v :Vex<CR>
nnoremap <leader>j :bnext<CR>
nnoremap <leader>k :bprev<CR>
nnoremap <leader>f za
" }}}

" Plugins {{{
" See https://github.com/junegunn/vim-plug/wiki/tips#automatic-installation
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
  " Visceral Inputs
  Plug 'windwp/nvim-autopairs' 
  Plug 'tpope/vim-commentary'
  Plug 'alvan/vim-closetag'

  " Telescope
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.0' }

  " Autocomplete
  Plug 'neoclide/coc.nvim', {'branch': 'release'}

  " Snippets
  Plug 'dcampos/nvim-snippy'

  " TypeScript
  Plug 'HerringtonDarkholme/yats.vim'
  Plug 'MaxMEllon/vim-jsx-pretty'

  " Colors
  Plug 'NLKNguyen/papercolor-theme'
call plug#end()
" }}}

" Autopairs {{{
lua << EOF
require("nvim-autopairs").setup {}
EOF
" }}}

" Coc Config {{{
set updatetime=300

inoremap <silent><expr> <C-n>
      \ coc#pum#visible() ? coc#pum#next(1) : coc#refresh()

inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

command! -nargs=0 Format :call CocAction('format')
nnoremap <leader>p :Format<cr>
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
" Use K to show documentation in preview window.
nnoremap <silent> K :call ShowDocumentation()<CR>
function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction
" }}}

" Vim Closetag {{{
let g:closetag_filenames = '*.html,*.jsx,*.tsx,*.erb'
" }}}

" Snippets {{{
lua << EOF
require('snippy').setup({
          mappings = {
              is = {
                  ['<Tab>'] = 'expand_or_advance',
                  ['<S-Tab>'] = 'previous',
              },
              nx = {
                  ['<leader>x'] = 'cut_text',
              },
          },
      })

EOF
" }}}

" Telescope {{{
nnoremap <leader>sf <cmd>Telescope find_files<cr>
nnoremap <leader>sg <cmd>Telescope live_grep<cr>
nnoremap <leader>sb <cmd>Telescope buffers<cr>
nnoremap <leader>sh <cmd>Telescope help_tags<cr>
" }}}

" Colorscheme {{{
set background=dark
colorscheme PaperColor
" }}}
