vim.g.mapleader = ' '

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup("j.plugins")

vim.o.background = "dark" -- or "light" for light mode
vim.cmd([[colorscheme gruvbox]])

vim.opt.clipboard = 'unnamedplus'
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2

-- nvim-tree binds 
vim.api.nvim_set_keymap('n', '<leader>e', ':NvimTreeToggle<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-h>', '<C-w>h', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>k', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-l>', '<C-w>l', { noremap = true, silent = true })

-- nvim-ale config
vim.cmd [[
  let g:ale_linters = {
      \ 'typescript': ['tsserver'],
      \ 'typescriptreact': ['tsserver'],
      \ }
  let g:ale_fixers = {
      \ 'typescript': ['prettier'],
      \ 'typescriptreact': ['prettier'],
      \ }
  let g:ale_fix_on_save = 1
]]

-- 将telescope快捷键绑定移到插件加载后
local telescope_setup = function()
  local telescope_loaded, builtin = pcall(require, 'telescope.builtin')
  if telescope_loaded then
    vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
    vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
    vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
    vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})
  end
end

-- 在VimEnter事件中设置telescope快捷键
vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    telescope_setup()
  end,
})

-- 自动重新加载配置
vim.cmd [[
  augroup config_reload
    autocmd!
    autocmd BufWritePost init.lua source <afile>
  augroup end
]]

-- 以下是旧的 packer 配置，现在使用 lazy.nvim 替代
-- require('packer').startup(function(use)
--   -- 核心插件
--   use 'wbthomason/packer.nvim'
  
--   -- LSP支持
--   use {
--     'VonHeikemen/lsp-zero.nvim',
--     requires = {
--       -- LSP Support
--       {'neovim/nvim-lspconfig'},
--       {'williamboman/mason.nvim'},
--       {'williamboman/mason-lspconfig.nvim'},
--
--       -- 自动补全
--       {'hrsh7th/nvim-cmp'},
--       {'hrsh7th/cmp-buffer'},
--       {'hrsh7th/cmp-path'},
--       {'hrsh7th/cmp-nvim-lsp'},
--       {'hrsh7th/cmp-nvim-lua'},
--     }
--   }
--
--   -- Copilot AI补全
--   use 'github/copilot.vim'
--
--   -- 文件模糊搜索
--   use {
--     'nvim-telescope/telescope.nvim',
--     requires = {'nvim-lua/plenary.nvim'}
--   }
--
--   -- 自动保存
--   use 'Pocco81/AutoSave.nvim'
--
--   -- 文件树
--   use {
--     'kyazdani42/nvim-tree.lua',
--     requires = {'kyazdani42/nvim-web-devicons'}
--   }
--
--   -- 代码高亮
--   use {
--     'nvim-treesitter/nvim-treesitter',
--     run = ':TSUpdate'
--   }
--
--   -- 状态栏
--   use {
--     'nvim-lualine/lualine.nvim',
--     requires = {'kyazdani42/nvim-web-devicons'}
--   }
--
--   -- Git集成
--   use 'lewis6991/gitsigns.nvim'
--
--   -- 与Tmux集成
--   use 'christoomey/vim-tmux-navigator'
--
--   -- 自动配对括号
--   use 'windwp/nvim-autopairs'
--
--   -- 注释插件
--   use 'numToStr/Comment.nvim'
--
--   if packer_bootstrap then
--     require('packer').sync()
--   end
-- end)

-- 基本设置
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.smartindent = true
vim.opt.termguicolors = true
vim.opt.cursorline = true
vim.opt.undofile = true
vim.opt.wrap = false
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.mouse = 'a'
vim.opt.clipboard = 'unnamedplus'
vim.opt.breakindent = true
vim.opt.updatetime = 250
vim.opt.timeoutlen = 300

-- 键盘映射
vim.g.mapleader = " "  -- 设置空格为leader键

-- Telescope快捷键
vim.api.nvim_set_keymap('n', '<leader>ff', '<cmd>Telescope find_files<cr>', {noremap = true})
vim.api.nvim_set_keymap('n', '<leader>fg', '<cmd>Telescope live_grep<cr>', {noremap = true})
vim.api.nvim_set_keymap('n', '<leader>fb', '<cmd>Telescope buffers<cr>', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-p>', '<cmd>Telescope find_files<cr>', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-g>', '<cmd>Telescope live_grep<cr>', {noremap = true})

-- NvimTree快捷键
vim.api.nvim_set_keymap('n', '<leader>e', '<cmd>NvimTreeToggle<cr>', {noremap = true})

-- 窗口导航快捷键
vim.api.nvim_set_keymap('n', '<C-h>', '<C-w>h', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>k', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-l>', '<C-w>l', {noremap = true})

-- 缓冲区导航
vim.api.nvim_set_keymap('n', '<S-l>', '<cmd>bnext<cr>', {noremap = true})
vim.api.nvim_set_keymap('n', '<S-h>', '<cmd>bprev<cr>', {noremap = true})

-- Copilot设置
vim.g.copilot_assume_mapped = true
vim.api.nvim_set_keymap('i', '<Tab>', 'copilot#Accept("<Tab>")', {expr = true, silent = true})

-- 延迟加载配置，确保插件已安装
-- AutoSave配置
vim.defer_fn(function()
  -- 检查插件是否存在
  local ok, autosave = pcall(require, 'autosave')
  if not ok then return end
  
  autosave.setup({
    enabled = true,
    execution_message = "AutoSave: saved at " .. vim.fn.strftime("%H:%M:%S"),
    events = {"InsertLeave", "TextChanged"},
    conditions = {
      exists = true,
      filename_is_not = {},
      filetype_is_not = {},
      modifiable = true
    },
    write_all_buffers = false,
    on_off_commands = true,
    clean_command_line_interval = 0,
    debounce_delay = 135
  })
end, 500)

-- LSP配置
vim.defer_fn(function()
  local ok, lsp = pcall(require, 'lsp-zero')
  if not ok then return end
  
  lsp.preset('recommended')
  lsp.setup()
end, 500)

-- 设置NvimTree
vim.defer_fn(function()
  local ok, nvim_tree = pcall(require, 'nvim-tree')
  if not ok then return end
  
  nvim_tree.setup()
end, 500)

-- 设置Treesitter
vim.defer_fn(function()
  local ok, treesitter = pcall(require, 'nvim-treesitter.configs')
  if not ok then return end
  
  treesitter.setup({
    ensure_installed = {"lua", "python", "javascript", "swift"},
    highlight = {
      enable = true,
    },
  })
end, 500)

-- 设置状态栏
vim.defer_fn(function()
  local ok, lualine = pcall(require, 'lualine')
  if not ok then return end
  
  lualine.setup({
    options = {
      theme = 'gruvbox',
      section_separators = {'', ''},
      component_separators = {'', ''}
    }
  })
end, 500)

-- 设置Git集成
vim.defer_fn(function()
  local ok, gitsigns = pcall(require, 'gitsigns')
  if not ok then return end
  
  gitsigns.setup()
end, 500)

-- 设置自动配对
vim.defer_fn(function()
  local ok, autopairs = pcall(require, 'nvim-autopairs')
  if not ok then return end
  
  autopairs.setup()
end, 500)

-- 设置注释插件
vim.defer_fn(function()
  local ok, comment = pcall(require, 'Comment')
  if not ok then return end
  
  comment.setup()
end, 500)
