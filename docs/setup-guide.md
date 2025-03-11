# Installation Guide

This guide will help you set up the same development environment as mine. The installation process is divided into several parts, following the recommended order.

*Note: Chinese users can also check the [Chinese version of this guide](setup-guide.zh.md).*

## Prerequisites

This setup is suitable for users familiar with terminals and command lines. If you're a beginner, you may need to learn some basics first.

## Required Dependencies

First, install the necessary dependencies:

```bash
# Install Homebrew (macOS package manager)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install necessary tools
brew install git emacs ripgrep fd tmux fswatch

# Install NeoVim
brew install neovim

# Install Karabiner-Elements and Raycast
brew install --cask karabiner-elements raycast
```

## 1. Install Doom Emacs

```bash
# Clone Doom Emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d

# Install Doom
~/.emacs.d/bin/doom install

# Create configuration directory
mkdir -p ~/.doom.d
```

## 2. Install Tmux Plugin Manager

```bash
# Install Tmux plugin manager
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

## 3. Prepare NeoVim Configuration Directory

```bash
# Create NeoVim configuration directory
mkdir -p ~/.config/nvim/lua
```

## 4. Prepare Aerospace Configuration

```bash
# Create Aerospace configuration directory
mkdir -p ~/.config/aerospace
```

## 5. Prepare Karabiner-Elements Configuration

```bash
# Create Karabiner configuration directory
mkdir -p ~/.config/karabiner
```

## 6. Create Scripts Directory

```bash
# Create bin directory
mkdir -p ~/bin
```

## 7. Install My Configuration

### Clone Configuration Repository

```bash
# Clone configuration repository
git clone https://github.com/drsc777/mydot.git ~/temp-dotfiles
```

Or, if you've already downloaded this repository, you can use it directly.

### Copy Configuration Files

```bash
# Copy Doom Emacs configuration
cp -r ~/temp-dotfiles/doom/* ~/.doom.d/

# Copy NeoVim configuration
cp -r ~/temp-dotfiles/nvim/* ~/.config/nvim/

# Copy Tmux configuration
cp ~/temp-dotfiles/tmux/.tmux.conf ~/.tmux.conf

# Copy Aerospace configuration
cp ~/temp-dotfiles/.aerospace.toml ~/.config/aerospace/config.toml

# Copy Karabiner configuration
cp -r ~/temp-dotfiles/karabiner/* ~/.config/karabiner/

# Copy utility scripts
cp ~/temp-dotfiles/bin/* ~/bin/
chmod +x ~/bin/*.sh
```

## 8. Apply Configuration

### Doom Emacs

```bash
# Sync Doom configuration
~/.emacs.d/bin/doom sync
```

### NeoVim

```bash
# Install plugins
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
```

### Tmux

In tmux, press `Ctrl+a` then `I` to install plugins.

### Set Up Auto-sync

```bash
# Add crontab task
(crontab -l 2>/dev/null; echo "*/15 * * * * ~/bin/sync-notes.sh >> ~/sync-notes.log 2>&1") | crontab -
```

### Set Shell Aliases

Add the following to your `~/.zshrc` or `~/.bashrc`:

```bash
# Custom commands
alias sync-cursor="~/bin/sync-cursor-nvim.sh"
alias sync-notes="~/bin/sync-notes.sh"

# Ensure bin directory is in PATH
export PATH="$HOME/bin:$PATH"
```

## 9. Create Notes Directory

```bash
# Create notes directory structure
mkdir -p ~/notes/{org,roam,journal,templates}
cd ~/notes
git init
echo "# My Notes System" > README.md
```

## 10. Set Up GitHub Repository (Optional)

If you want to automatically sync notes to GitHub:

1. Create a new repository on GitHub (e.g., "notes")
2. Add the remote link:

```bash
cd ~/notes
git remote add origin https://github.com/yourusername/notes.git
git add .
git commit -m "Initial commit"
git push -u origin main
```

## 11. Configure Karabiner-Elements

1. Open Karabiner-Elements
2. Go to "Complex Modifications" tab
3. Click "Add rule"
4. Enable the Vim navigation rules

## 12. Configure Raycast

1. Open Raycast (default shortcut is Command+Space)
2. Follow the setup wizard
3. Change the activation shortcut to Option+Space (to avoid conflict with Spotlight)
4. Enable Window Management extension
5. Configure keyboard shortcuts as described in `~/temp-dotfiles/raycast/raycast-config.md`

## 13. Configure Vim for Cursor

To optimize Cursor for Vim keybindings:

1. Run the Cursor key repeat setup script:
   ```bash
   ~/mydot/cursor/setup_keyrepeat.sh
   ```

2. This script will:
   - Optimize system-wide key repeat settings
   - Configure Cursor-specific settings
   - Install Vim configuration for Cursor

3. Restart Cursor to apply the changes

## 14. Install Fonts (Recommended)

For the best experience, install a programming font:

```bash
brew tap homebrew/cask-fonts
brew install --cask font-fira-code
```

## Done!

Congratulations! You have now set up a complete mouse-free development environment. Please refer to the [Keyboard Shortcuts Guide](keyboard-shortcuts.md) to learn how to use this toolset. 