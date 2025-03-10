# Configuration Files Explanation

This document explains in detail the content of various tool configuration files, helping you understand the purpose of each setting.

## Doom Emacs Configuration

### config.el

Main configuration file, including personal information, themes, feature settings, etc:

```elisp
;; Personal information
(setq user-full-name "abby"
      user-mail-address "abbyzyl777@gmail.com")

;; Theme setup
(setq doom-theme 'doom-gruvbox)  ;; Using Gruvbox theme

;; Line number display
(setq display-line-numbers-type t)

;; Notes directory setup
(setq org-directory "~/notes/org/")
(setq org-roam-directory (file-truename "~/notes/roam/"))
(setq org-roam-file-extensions '("org"))

;; Org Roam display template
(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

;; 笔记捕获模板
(setq org-roam-capture-templates
      '(("m" "main" plain "%?" ...)  ;; 主要笔记
        ("r" "reference" plain "%?" ...) ;; 参考资料笔记
        ("a" "article" plain "%?" ...) ;; 文章笔记
        ("d" "default" plain "%?" ...))) ;; 默认笔记

;; Org Roam 配置
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/notes/roam/"))
  :bind (...)
  :config ...)

;; Org Roam UI 配置
(use-package! org-roam-ui
  :after org-roam
  :hook (org-roam-mode . org-roam-ui-mode)
  :config ...)

;; 终端支持
(use-package vterm
  :ensure t
  :commands vterm)

;; 自动折叠
(after! org
  (setq org-startup-folded t))

;; TODO状态设置
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)"))))

;; Org Babel支持
(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (emacs-lisp . t))))

;; 自动保存
(use-package! super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; GitHub自动同步
(defun sync-org-notes ()
  (when (eq major-mode 'org-mode)
    (let ((default-directory "~/notes/"))
      (when (file-exists-p ".git")
        (shell-command "git add . && git commit -m 'Auto-sync notes' && git push origin main || true")))))

(add-hook 'after-save-hook 'sync-org-notes)

;; 日志配置
(use-package! org-journal
  :config
  (setq org-journal-dir "~/notes/journal/"
        org-journal-date-format "%Y-%m-%d"))

;; 快速记录设置
(after! org
  (setq org-capture-templates
        '(...)))
```

### init.el

Module configuration file, controls which Doom modules are enabled:

```elisp
(doom! :input
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       company             ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ivy                 ; a search engine for love and life
       vertico             ; the search engine of the future

       :ui
       ;;deft              ; notational velocity for Emacs
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       doom-quit           ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline            ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints             ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       treemacs            ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter           ; vcs diff in the fringe
       vi-tilde-fringe     ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces          ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets            ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired               ; making dired pretty [functional]
       electric            ; smarter, keyword-based electric-indent
       ;;ibuffer           ; interactive buffer management
       undo                ; persistent, smarter undo for your inevitable mistakes
       vc                  ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm               ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;(spell +flyspell) ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       lookup              ; navigate your code and its documentation
       ;;lsp               ; M-x vscode
       magit               ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       ;;cc                ; C > C++ == 1
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp          ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;json              ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       ;;lua               ; one-based indices? one-based indices
       markdown            ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       org                 ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       python              ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       sh                  ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))
```

### packages.el

Package management file, specifies which additional packages to install:

```elisp
(package! org-roam)
(package! org-journal)
(package! deft)
(package! org-roam-ui)
(package! visual-fill-column)
(package! org-bullets)
(package! org-super-agenda)
(package! org-fancy-priorities)
```

## NeoVim Configuration

### init.lua

Main configuration file:

```lua
-- Set leader key to space
vim.g.mapleader = " "

-- Basic settings
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.wrap = false
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.scrolloff = 8
vim.opt.updatetime = 50
vim.opt.colorcolumn = "80"

-- Auto-install packer if not installed
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

-- Plugin setup
require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'
  
  -- Color scheme
  use 'gruvbox-community/gruvbox'
  
  -- Telescope for fuzzy finding
  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }
  
  -- Treesitter for better syntax highlighting
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }
  
  -- LSP configuration
  use 'neovim/nvim-lspconfig'
  
  -- Completion
  use 'hrsh7th/nvim-cmp'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/cmp-path'
  
  -- Snippets
  use 'L3MON4D3/LuaSnip'
  use 'saadparwaiz1/cmp_luasnip'
  
  -- File explorer
  use 'kyazdani42/nvim-tree.lua'
  use 'kyazdani42/nvim-web-devicons'
  
  -- Status line
  use 'nvim-lualine/lualine.nvim'
  
  -- Git integration
  use 'lewis6991/gitsigns.nvim'
  
  -- Auto pairs
  use 'windwp/nvim-autopairs'
  
  -- Comments
  use 'numToStr/Comment.nvim'
  
  -- Copilot
  use 'github/copilot.vim'
  
  if packer_bootstrap then
    require('packer').sync()
  end
end)

-- Plugin configuration
require('telescope').setup{}
require('nvim-treesitter.configs').setup {
  highlight = {
    enable = true,
  },
}

-- LSP setup
local lspconfig = require('lspconfig')
lspconfig.pyright.setup{}
lspconfig.tsserver.setup{}

-- Completion setup
local cmp = require('cmp')
cmp.setup({
  snippet = {
    expand = function(args)
      require('luasnip').lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  }, {
    { name = 'buffer' },
  })
})

-- File explorer setup
require('nvim-tree').setup{}

-- Status line setup
require('lualine').setup{
  options = {
    theme = 'gruvbox',
  }
}

-- Git signs setup
require('gitsigns').setup()

-- Auto pairs setup
require('nvim-autopairs').setup{}

-- Comments setup
require('Comment').setup()

-- Key mappings
-- Telescope
vim.keymap.set('n', '<leader>ff', ':Telescope find_files<CR>')
vim.keymap.set('n', '<leader>fg', ':Telescope live_grep<CR>')
vim.keymap.set('n', '<leader>fb', ':Telescope buffers<CR>')
vim.keymap.set('n', '<leader>fh', ':Telescope help_tags<CR>')
-- NvimTree
vim.keymap.set('n', '<leader>e', ':NvimTreeToggle<CR>')
-- Save and quit
vim.keymap.set('n', '<leader>w', ':w<CR>')
vim.keymap.set('n', '<leader>q', ':q<CR>')
-- Clear search highlight
vim.keymap.set('n', '<leader>h', ':nohlsearch<CR>')
-- Navigate splits
vim.keymap.set('n', '<C-h>', '<C-w>h')
vim.keymap.set('n', '<C-j>', '<C-w>j')
vim.keymap.set('n', '<C-k>', '<C-w>k')
vim.keymap.set('n', '<C-l>', '<C-w>l')

-- Theme setup
vim.cmd [[
  colorscheme gruvbox
  set background=dark
]]

-- Auto commands
vim.cmd [[
  " Auto-format on save
  augroup fmt
    autocmd!
    autocmd BufWritePre * undojoin | Neoformat
  augroup END
  
  " Remember last position
  augroup remember_position
    autocmd!
    autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
  augroup END
]]
```

## Tmux Configuration

### .tmux.conf

```bash
# Set prefix to Ctrl+a
unbind C-b
set -g prefix C-a
bind C-a send-prefix

# Split panes using | and -
bind | split-window -h -c "#{pane_current_path}"
bind - split-window -v -c "#{pane_current_path}"
unbind '"'
unbind %

# Reload config file
bind r source-file ~/.tmux.conf \; display "Config reloaded!"

# Enable mouse mode
set -g mouse on

# Use vim keybindings in copy mode
setw -g mode-keys vi

# Set up 'v' to begin selection as in Vim
bind-key -T copy-mode-vi v send-keys -X begin-selection

# Smart pane switching with awareness of Vim splits
bind -n C-h run "(tmux display-message -p '#{pane_current_command}' | grep -iq vim && tmux send-keys C-h) || tmux select-pane -L"
bind -n C-j run "(tmux display-message -p '#{pane_current_command}' | grep -iq vim && tmux send-keys C-j) || tmux select-pane -D"
bind -n C-k run "(tmux display-message -p '#{pane_current_command}' | grep -iq vim && tmux send-keys C-k) || tmux select-pane -U"
bind -n C-l run "(tmux display-message -p '#{pane_current_command}' | grep -iq vim && tmux send-keys C-l) || tmux select-pane -R"

# Resize panes
bind -r H resize-pane -L 10
bind -r J resize-pane -D 10
bind -r K resize-pane -U 10
bind -r L resize-pane -R 10

# Set terminal color
set -g default-terminal "screen-256color"
set -ga terminal-overrides ",*256col*:Tc"

# Start windows and panes at 1, not 0
set -g base-index 1
setw -g pane-base-index 1

# Don't rename windows automatically
set-option -g allow-rename off

# Set status bar
set -g status-style fg=white,bg=black
set -g window-status-current-style fg=green,bold
set -g status-interval 60
set -g status-left-length 30
set -g status-left '#[fg=green](#S) '
set -g status-right '#[fg=yellow]%H:%M#[default]'

# Set window title
set-window-option -g automatic-rename on
set-option -g set-titles on

# Enable focus events
set -g focus-events on

# Set escape time
set -sg escape-time 0

# TPM plugins
set -g @plugin 'tmux-plugins/tpm'
set -g @plugin 'tmux-plugins/tmux-sensible'
set -g @plugin 'tmux-plugins/tmux-resurrect'
set -g @plugin 'tmux-plugins/tmux-continuum'
set -g @plugin 'tmux-plugins/tmux-yank'
set -g @plugin 'christoomey/vim-tmux-navigator'

# Initialize TPM
run '~/.tmux/plugins/tpm/tpm'
```

## Aerospace Configuration

### config.toml

```toml
# Aerospace Configuration
name = "aerospace"

# General settings
[general]
default_workspace = "1"
enable_hot_keys = true
focus_follows_mouse = false
mouse_mod = "super"
hot_reload = true

# Appearance settings
[appearance]
border_width = 2
gap_width = 10
border_focused = "#d79921"
border_unfocused = "#928374"

# Key bindings
[keys]
workspaces = { alt = ["1", "2", "3", "4", "5", "6", "7", "8", "9"] }
move_to_workspace = { alt_shift = ["1", "2", "3", "4", "5", "6", "7", "8", "9"] }
navigate = { alt = ["h", "j", "k", "l"] }
resize = { alt_shift = ["h", "j", "k", "l"] }
toggle_fullscreen = { alt = ["f"] }
toggle_float = { alt_shift = ["f"] }
center_float = { alt_shift = ["space"] }
balance = { alt_shift = ["e"] }
quit = { alt_shift = ["q"] }
reload_config = { alt_shift = ["r"] }

# Layout settings
[layouts]
default = "main_and_deck"
```

## Utility Scripts

### sync-cursor-nvim.sh

Script to synchronize Cursor and NeoVim:

```bash
#!/bin/bash

# Set project directory
PROJECT_DIR=$1
if [ -z "$PROJECT_DIR" ]; then
  echo "Please specify the project directory to monitor"
  exit 1
fi

# Clean up old processes
pkill -f "fswatch -o $PROJECT_DIR"

# Start bidirectional sync
echo "Starting to monitor file changes in $PROJECT_DIR..."

# Monitor NeoVim changes and notify Cursor
fswatch -o "$PROJECT_DIR" | xargs -n1 -I{} osascript -e 'tell application "Cursor" to activate' &

# Log process ID
echo "Sync process started, PID: $!" 
```

### sync-notes.sh

Script to automatically sync notes to GitHub:

```bash
#!/bin/bash

# Set notes directory
NOTES_DIR=~/notes

# If directory doesn't exist, create it
if [ ! -d "$NOTES_DIR" ]; then
  echo "Creating notes directory: $NOTES_DIR"
  mkdir -p "$NOTES_DIR"
fi

cd "$NOTES_DIR"

# If not a git repository, initialize it
if [ ! -d ".git" ]; then
  echo "Initializing Git repository"
  git init
  echo "# My Notes" > README.md
  git add README.md
  git commit -m "Initialize notes repository"
  
  echo "Please manually set up remote repository, for example:"
  echo "git remote add origin https://github.com/yourusername/notes.git"
  exit 0
fi

# Check if there are changes
if [[ -z $(git status -s) ]]; then
  echo "No changes to sync"
  exit 0
fi

# Add all changes
git add .

# Commit changes
git commit -m "Auto-sync notes: $(date +'%Y-%m-%d %H:%M:%S')"

# Try to push to remote
if git remote | grep -q origin; then
  git push origin main || git push origin master || echo "Push failed, may need to set up remote repository"
else
  echo "Please set up remote repository first, for example:"
  echo "git remote add origin https://github.com/yourusername/notes.git"
fi
```

*Note: Chinese users can also check the [Chinese version of this guide](config-explanation.zh.md).* 