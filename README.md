# Abby's Development Environment Configuration (dotfiles)

This repository contains my complete development environment configuration, based on a mouse-free philosophy, focused on efficient programming experience.

[中文文档 (Chinese Version)](README.zh.md)

## Quick Installation

```bash
# Clone repository
git clone https://github.com/drsc777/mydot.git
cd mydot

# Run installation script
./install_en.sh # English version
./install.sh    # Chinese version
```

## Environment Overview

My development environment consists of the following components:

- **Doom Emacs** - Note management and Org Mode
- **NeoVim** - Primary coding tool
- **Cursor AI** - AI-assisted programming
- **Tmux** - Terminal session management
- **Aerospace** - Window manager (macOS)

## Directory Structure

```
mydot/
├── doom/                 # Doom Emacs configuration
│   ├── config.el         # Main configuration
│   ├── init.el           # Initialization and modules
│   ├── packages.el       # Package management
│   └── custom.el         # Auto-generated custom configuration
├── nvim/                 # NeoVim configuration
│   ├── init.lua          # Main configuration file
│   └── lua/              # Lua modules
├── tmux/                 # Tmux configuration
│   └── .tmux.conf        # Main configuration file
├── bin/                  # Utility scripts
│   ├── sync-cursor-nvim.sh  # Cursor AI and NeoVim synchronization
│   └── sync-notes.sh     # Auto-sync notes to GitHub
├── .aerospace.toml      # Aerospace window manager configuration
└── docs/                 # Documentation
    ├── setup-guide.md    # Installation guide (Chinese)
    ├── setup-guide-en.md # Installation guide (English)
    └── keyboard-shortcuts.md # Keyboard shortcuts reference
```

## Key Features

1. **Mouse-free Workflow**: All tools are configured to ensure complete keyboard operation
2. **AI-enhanced Programming**: Integration with GitHub Copilot and Cursor AI
3. **Note Management System**: Knowledge base built on Org-roam
4. **Auto Synchronization**: Notes automatically committed to GitHub
5. **Efficient Window Management**: Seamless window operations with Tmux and Aerospace

## Installation Guide

For detailed installation steps, please check the [Installation Guide](docs/setup-guide-en.md)

## Keyboard Shortcuts Reference

For a complete list of all important keyboard shortcuts, please refer to the [Keyboard Shortcuts Guide](docs/keyboard-shortcuts.md)

## Documentation

- All documentation supports both Chinese and English
- Installation scripts are available in both Chinese and English (`install.sh` and `install_en.sh`)
- A fully English installation guide is provided at `docs/setup-guide-en.md`

## Configuration Files Explanation

- **Doom Emacs**: Configured with Org-mode, auto-folding, TODO management, note linking, etc.
- **NeoVim**: Integrated with LSP, Treesitter, Telescope, auto-save, etc.
- **Tmux**: Configured with Vim-style navigation, session restoration, and window management
- **Scripts**: Provides note synchronization and Cursor AI integration

## License

MIT 
