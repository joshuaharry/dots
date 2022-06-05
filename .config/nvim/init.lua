-- Default Settings {{{
vim.opt.syntax = "on"
vim.opt.number = true
vim.opt.foldmethod = "marker"
vim.opt.encoding = "utf-8"
vim.opt.clipboard = "unnamed"
vim.opt.mouse = "a"
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.background = "dark"
vim.opt.completeopt = "menu"
-- }}}

-- Keybindings {{{
vim.g.mapleader = " "
vim.api.nvim_set_keymap("n", "<leader>e", "Ex!<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader>v", "Vex!<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader>j", ":bnext<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader>k", ":bprev<CR>", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader>f", "za", { noremap = true })
vim.api.nvim_set_keymap("n", "<leader>w", "<C-w>w", { noremap = true })
vim.api.nvim_set_keymap("n", "q", ":q<CR>", { noremap = true})
-- }}}

-- Plugins {{{
local packer_install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(packer_install_path)) > 0 then
	packer_bootstrap = vim.fn.system({
		'git', 
		'clone', 
		'--depth', 
		'1', 
		'https://github.com/wbthomason/packer.nvim', 
		packer_install_path
	})
	-- We need to reset the runtime path; otherwise, Neovim doesn't pick up on
	-- what we just cloned.
	vim.cmd [[ set runtimepath& ]]
end

local packer = require('packer').startup(function(use)
    use "wbthomason/packer.nvim"

    -- Generic programming features
    use {
            'numToStr/Comment.nvim',
            config = function()
		            if not packer_bootstrap then require('Comment').setup() end
            end
    }
    use {
            "windwp/nvim-autopairs",
            config = function()
        	    if not packer_bootstrap then require('nvim-autopairs').setup{} end
            end
    }

    -- Theming
    use {
      "NLKNguyen/papercolor-theme",
      config = function()
        if not packer_bootstrap then 
          vim.cmd [[ colorscheme PaperColor ]] 
        end
      end
   }
    
    -- TypeScript/React support
    use "HerringtonDarkholme/yats.vim"
    use "MaxMEllon/vim-jsx-pretty"
    use {
      "alvan/vim-closetag",
      config = function() 
        vim.cmd [[ let g:closetag_filenames = '*.html,*.jsx,*.tsx' ]]
      end
    }

    -- Fish support
    use "dag/vim-fish"

    -- LSP Configuration
    use {
      "neoclide/coc.nvim",
      branch = 'release',
      config = function() 
        if not packer_bootstrap then
          vim.cmd [[
             inoremap <silent><expr> <C-n> coc#refresh()
             set updatetime=300
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
          ]]
        end
      end
    }
    use {
    "dcampos/nvim-snippy",
    config = function() 
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
    end
    }
    use {
        'nvim-telescope/telescope.nvim',
        requires = { {'nvim-lua/plenary.nvim'} },
        config = function()
          vim.cmd [[
            nnoremap <leader>sf <cmd>Telescope find_files<cr>
            nnoremap <leader>sg <cmd>Telescope live_grep<cr>
            nnoremap <leader>sb <cmd>Telescope buffers<cr>
            nnoremap <leader>sh <cmd>Telescope help_tags<cr>
          ]]
        end
    }
    use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
    use "ap/vim-css-color"
end)

if packer_bootstrap then
   packer.sync()
end
-- }}}
