# Complete Beginner's Guide to Mouse-Free Development Environment

Welcome to the comprehensive beginner's guide for setting up and using a keyboard-centric development environment. This guide will walk you through installing and configuring all tools in this dotfiles repository, with clear explanations for each step.

## Table of Contents

1. [Introduction](#introduction)
2. [Prerequisites](#prerequisites)
3. [Installation](#installation)
4. [Understanding Your New Setup](#understanding-your-new-setup)
5. [Doom Emacs Basics](#doom-emacs-basics)
6. [Neovim Basics](#neovim-basics)
7. [Tmux Basics](#tmux-basics)
8. [Aerospace: Window Management](#aerospace-window-management)
9. [Karabiner-Elements: Keyboard Customization](#karabiner-elements-keyboard-customization)
10. [Raycast: Launcher and Utilities](#raycast-launcher-and-utilities)
11. [Common Workflows](#common-workflows)
12. [Troubleshooting](#troubleshooting)
13. [Further Resources](#further-resources)

## Introduction

This environment creates a "mouse-free" development setup focused on maximizing productivity through keyboard shortcuts. The key components are:

- **Doom Emacs**: A configuration framework for Emacs with modal editing
- **Neovim**: A modern, improved version of Vim
- **Tmux**: Terminal multiplexer for managing multiple terminal sessions
- **Aerospace**: Window manager for macOS
- **Karabiner-Elements**: Tool for remapping keys and creating complex modifications
- **Raycast**: Productivity tool to replace Spotlight search

By the end of this guide, you'll have a fully configured development environment and understand how to use each component effectively.

## Prerequisites

Before starting, ensure you have:

- A Mac running macOS (Big Sur or newer recommended)
- Administrator access to install software
- Basic terminal familiarity (knowing how to open Terminal.app is sufficient)
- At least 5GB of free disk space
- A stable internet connection
- 1-2 hours of time to complete the setup

## Installation

### Automated Installation

For a quick setup, you can use the automated installation script:

1. Open Terminal (press Cmd+Space, type "Terminal", press Enter)
2. Run the following command:

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/drsc777/mydot/main/install.sh)"
```

3. Follow the on-screen prompts
4. When it completes, restart your terminal

### Manual Installation

If you prefer to understand each step or the automatic installation doesn't work, follow these steps:

#### 1. Install Homebrew

Homebrew is a package manager for macOS that makes installing software easy:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

After installation, follow the instructions to add Homebrew to your PATH.

#### 2. Install Core Dependencies

```bash
# Install command-line tools
brew install git emacs neovim tmux ripgrep fd fswatch

# Install GUI applications
brew install --cask karabiner-elements raycast
```

#### 3. Clone the Configuration Repository

```bash
# Create a temporary directory for the dotfiles
mkdir -p ~/Desktop/mydot
cd ~/Desktop/mydot

# Clone the repository
git clone https://github.com/drsc777/mydot.git .
```

#### 4. Set Up Doom Emacs

```bash
# Clone Doom Emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d

# Install Doom
~/.emacs.d/bin/doom install

# Copy the Doom configuration
mkdir -p ~/.doom.d
cp -r ~/Desktop/mydot/doom/* ~/.doom.d/

# Sync Doom configuration
~/.emacs.d/bin/doom sync
```

#### 5. Set Up Neovim

```bash
# Create Neovim configuration directory
mkdir -p ~/.config/nvim/lua

# Copy Neovim configuration
cp -r ~/Desktop/mydot/nvim/* ~/.config/nvim/

# Install Neovim plugins (this may take a few minutes)
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
```

#### 6. Set Up Tmux

```bash
# Install Tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# Copy Tmux configuration
cp ~/Desktop/mydot/tmux/.tmux.conf ~/.tmux.conf
```

#### 7. Set Up Aerospace (Window Manager)

```bash
# Create configuration directory
mkdir -p ~/.config/aerospace

# Copy configuration
cp ~/Desktop/mydot/aerospace/.aerospace.toml ~/.config/aerospace/config.toml
```

#### 8. Set Up Utility Scripts

```bash
# Create bin directory
mkdir -p ~/bin

# Copy utility scripts
cp ~/Desktop/mydot/bin/* ~/bin/
chmod +x ~/bin/*.sh
```

#### 9. Configure Shell Integration

Add the following to your ~/.zshrc or ~/.bashrc:

```bash
# Custom aliases
alias sync-cursor="~/bin/sync-cursor-nvim.sh"
alias sync-notes="~/bin/sync-notes.sh"

# Ensure bin directory is in PATH
export PATH="$HOME/bin:$PATH"
```

#### 10. Set Up Notes System

```bash
mkdir -p ~/notes/{org,roam,journal}
cd ~/notes
git init
echo "# My Notes System" > README.md
git add README.md
git commit -m "Initialize notes repository"
```

## Understanding Your New Setup

### System Components Overview

Your new development environment consists of these integrated components:

1. **Terminal and Shell**: The foundation where most tools run
2. **Tmux**: Creates multiple terminal sessions in one window
3. **Doom Emacs**: Text editor with Vim-like keybindings and IDE features
4. **Neovim**: Lightweight code editor for quick edits
5. **Aerospace**: Controls window positioning and workspaces
6. **Karabiner-Elements**: Customizes keyboard behavior
7. **Raycast**: Provides quick access to commands and applications

### Configuration Files Location

All configuration files ("dotfiles") are stored in specific locations:

- Doom Emacs: `~/.doom.d/`
- Neovim: `~/.config/nvim/`
- Tmux: `~/.tmux.conf`
- Aerospace: `~/.config/aerospace/config.toml`
- Karabiner: `~/.config/karabiner/`
- Utility scripts: `~/bin/`

## Doom Emacs Basics

### Starting Doom Emacs

To start Doom Emacs, open Terminal and type:

```bash
emacs
```

### Key Concepts

- **Modal Editing**: Like Vim, Doom has different modes (normal, insert, visual)
- **Leader Key**: Space is the "leader key" that triggers most commands
- **Which-key**: Menus appear showing available commands

### Essential Commands

- `ESC` or `Ctrl+g`: Cancel/return to normal mode
- `i`: Enter insert mode (for typing)
- `Space .`: Find file
- `Space ,`: Switch buffer
- `Space :`: Run command (like M-x)
- `Space f s`: Save file
- `Space q q`: Quit Emacs

### Org Mode Basics

Org mode is a powerful note-taking and organization system:

- Create new headings with `*` (more stars for sub-headings)
- `Tab` on a heading: Cycle through folded states
- `Shift+Tab`: Cycle all headings
- `Ctrl+Enter`: Create new heading
- `Alt+Enter`: Create new item at current level
- `Space m a`: Add an action/todo item

### Org Roam Basics

Org Roam creates a network of linked notes:

- `Space n r f`: Find or create a note
- `Space n r i`: Insert a link to another note
- `Space n r b`: Show backlinks buffer

## Neovim Basics

### Starting Neovim

To start Neovim, open Terminal and type:

```bash
nvim
```

### Key Concepts

Neovim uses the same modal editing paradigm as Vim:

- **Normal mode**: For navigation and commands
- **Insert mode**: For typing text
- **Visual mode**: For selecting text
- **Command mode**: For entering commands with `:` prefix

### Essential Commands

- `i`: Enter insert mode
- `Esc`: Return to normal mode
- `:w`: Save file
- `:q`: Quit (`:wq` to save and quit)
- `/search term`: Search forward
- `n`: Find next match
- `Space e`: Toggle file explorer
- `Space ff`: Find files
- `Space fg`: Live grep search
- `Ctrl+h/j/k/l`: Navigate between splits

## Tmux Basics

### Starting Tmux

To start a new Tmux session:

```bash
tmux
```

### Key Concepts

- **Prefix Key**: `Ctrl+a` is the prefix (must be pressed before other commands)
- **Sessions**: Independent terminal environments
- **Windows**: Tabs within a session
- **Panes**: Split areas within a window

### Essential Commands

- `Ctrl+a c`: Create new window
- `Ctrl+a ,`: Rename current window
- `Ctrl+a n`: Next window
- `Ctrl+a p`: Previous window
- `Ctrl+a %`: Split vertically
- `Ctrl+a "`: Split horizontally
- `Ctrl+a arrows`: Navigate panes
- `Ctrl+a d`: Detach from session
- `tmux attach`: Reattach to session

## Aerospace: Window Management

### Key Concepts

Aerospace automatically manages window positions on your screen.

### Essential Commands

- `Option+Enter`: Toggle fullscreen
- `Option+h/j/k/l`: Move focus between windows
- `Option+Shift+h/j/k/l`: Move window
- `Option+1-9`: Switch to workspace
- `Option+Shift+1-9`: Move window to workspace

## Karabiner-Elements: Keyboard Customization

### Key Concepts

Karabiner remaps keys for improved ergonomics and efficiency.

### Important Modifications

- Caps Lock acts as Escape when tapped, Control when held
- Right Option becomes Hyper key (Ctrl+Shift+Cmd+Option)

## Raycast: Launcher and Utilities

### Key Concepts

Raycast replaces Spotlight with enhanced features.

### Essential Commands

- `Option+Space`: Open Raycast
- Start typing to search applications, files, or actions
- `Option+->`: View extensions and actions for found items

## Common Workflows

### Writing and Note-taking

1. Open Emacs (`emacs`)
2. Access Org Roam (`Space n r f`)
3. Type note name or create a new one
4. Write content using Org Mode formatting
5. Save with `Space f s`

### Coding Session

1. Start Tmux (`tmux`)
2. Create a window for code (`Ctrl+a c`)
3. Open Neovim (`nvim project-file.py`)
4. Create another window for terminal (`Ctrl+a c`)
5. Switch between windows with `Ctrl+a n` and `Ctrl+a p`
6. Edit code in Neovim, run in terminal

### Window Management

1. Open applications needed for your task
2. Use `Option+h/j/k/l` to navigate between windows
3. Use `Option+Shift+h/j/k/l` to arrange them
4. Use `Option+1-9` to organize into workspaces

## Troubleshooting

### Common Issues and Solutions

1. **Emacs shows strange characters**
   - Ensure you've installed a compatible font with `brew install --cask font-fira-code`

2. **Neovim plugins not working**
   - Run `:checkhealth` in Neovim to diagnose
   - Ensure Node.js is installed with `brew install node`

3. **Tmux shows "invalid prefix key" error**
   - Ensure `.tmux.conf` is correctly copied to your home directory

4. **Karabiner not working**
   - Ensure it has accessibility permissions in System Preferences

### Getting Help

If you encounter issues:

1. Check the troubleshooting section of README.md
2. Look for error messages and search online
3. Review the specific tool's documentation
4. Open an issue on the GitHub repository

## Further Resources

### Learning Resources

- Doom Emacs Documentation: https://github.com/doomemacs/doomemacs/blob/master/docs/index.org
- Neovim Documentation: https://neovim.io/doc/
- Tmux Cheat Sheet: https://tmuxcheatsheet.com/
- Org Mode Manual: https://orgmode.org/manual/

### Community Support

- Doom Emacs Discord: https://discord.gg/qvGgnVx
- Neovim Discourse: https://neovim.discourse.group/
- Reddit Communities: r/emacs, r/vim, r/neovim

---

Remember, mastering this environment takes time. Focus on learning a few commands each day rather than trying to memorize everything at once. With practice, these tools will become second nature and significantly boost your productivity. 