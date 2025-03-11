# Abby's Development Environment Configuration (dotfiles)

This repository contains my complete development environment configuration, based on a mouse-free philosophy, focused on efficient programming experience.

[中文文档 (Chinese Version)](zh.docs/README.zh.md)

## Quick Installation

```bash
# Clone repository
git clone https://github.com/drsc777/mydot.git
cd mydot

# Run installation script
./install.sh    # Installation script
```

## Environment Overview

My development environment consists of the following components:

- **Doom Emacs** - Note management and Org Mode
- **NeoVim** - Primary coding tool
- **Cursor AI** - AI-assisted programming
- **Tmux** - Terminal session management
- **Aerospace** - Window manager (macOS)
- **Karabiner-Elements** - Global Vim-style keyboard shortcuts
- **Raycast** - Launcher and productivity tool
- **XVim2** - Vim keybindings for Xcode

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
├── karabiner/            # Karabiner-Elements configuration
│   ├── karabiner.json    # Main configuration file
│   └── vim_navigation.json # Vim-style navigation rules
├── raycast/              # Raycast configuration guide
│   └── raycast-config.md # Setup instructions
├── xvim/                 # XVim2 configuration for Xcode
│   ├── xvimrc            # XVim configuration file
│   └── install_xvim.sh   # XVim installation script
├── cursor/               # Cursor app configuration
│   ├── cursor_vim_settings.json # Vim settings for Cursor
│   └── setup_keyrepeat.sh # Script to optimize key repeat
├── .aerospace.toml      # Aerospace window manager configuration
└── docs/                 # Documentation
    ├── setup-guide.md    # Installation guide (English)
    ├── keyboard-shortcuts.md    # Keyboard shortcuts reference (English)
    └── config-explanation.md    # Configuration explanation (English)
```

## Key Features

1. **Mouse-free Workflow**: All tools are configured to ensure complete keyboard operation
2. **AI-enhanced Programming**: Integration with GitHub Copilot and Cursor AI
3. **Note Management System**: Knowledge base built on Org-roam
4. **Auto Synchronization**: Notes automatically committed to GitHub
5. **Efficient Window Management**: Seamless window operations with Tmux and Aerospace
6. **Global Vim Navigation**: Use Vim-style keys across all applications with Karabiner
7. **Quick App Launching**: Fast application access with Raycast
8. **Vim in Xcode**: XVim2 integration for Vim keybindings in Xcode
9. **Optimized Key Repeat**: Improved key repeat settings for Vim operations

## Installation Guide

For detailed installation steps, please check the [Installation Guide](docs/setup-guide.md)

## Keyboard Shortcuts Reference

For a complete list of all important keyboard shortcuts, please refer to the [Keyboard Shortcuts Guide](docs/keyboard-shortcuts.md)

## Documentation

- All documentation is available in both English and Chinese
- Each document has two versions:
  - Main version (English): `filename.md` in `docs/` directory
  - Chinese version: `filename.zh.md` in `zh.docs/` directory
- Installation script is available in both languages in the `install.sh` file

## Configuration Files Explanation

For a detailed explanation of all configuration files, see the [Configuration Files Guide](docs/config-explanation.md)

## Notes System

This configuration works seamlessly with my [notes system](https://github.com/drsc777/notes) for knowledge management.

Key features:
- Automatic sync every 15 minutes
- Org-roam for networked note-taking
- Task management with TODO system
- Journal for daily logging

## License

MIT 
