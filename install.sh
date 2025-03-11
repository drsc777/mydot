#!/bin/bash

# One-click installation script
# Usage: ./install_en.sh

# Set colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # Reset color

echo -e "${BLUE}Starting mouse-free development environment setup...${NC}"

# Check dependencies
echo -e "${BLUE}Checking dependencies...${NC}"
if ! command -v brew &> /dev/null; then
    echo -e "${RED}Homebrew not detected, installing...${NC}"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
else
    echo -e "${GREEN}Homebrew already installed.${NC}"
fi

# Install necessary packages
echo -e "${BLUE}Installing necessary tools...${NC}"
brew install git emacs neovim tmux ripgrep fd fswatch || true

# Install Karabiner-Elements and Raycast
echo -e "${BLUE}Installing Karabiner-Elements and Raycast...${NC}"
brew install --cask karabiner-elements raycast || true

# Check if Cursor is installed
if [ -d "/Applications/Cursor.app" ]; then
    echo -e "${BLUE}Cursor detected, setting up Vim mode and key repeat...${NC}"
    ~/mydot/cursor/setup_keyrepeat.sh
else
    echo -e "${BLUE}Cursor not detected, skipping Cursor Vim setup.${NC}"
    echo -e "${GREEN}To set up Cursor Vim mode after installing Cursor, run:${NC}"
    echo -e "${GREEN}~/mydot/cursor/setup_keyrepeat.sh${NC}"
fi

# Create necessary directories
echo -e "${BLUE}Creating necessary directories...${NC}"
mkdir -p ~/.doom.d
mkdir -p ~/.config/nvim/lua
mkdir -p ~/.config/aerospace
mkdir -p ~/.config/karabiner
mkdir -p ~/bin
mkdir -p ~/notes/{org,roam,journal,templates}

# Initialize notes repository
echo -e "${BLUE}Initializing notes repository...${NC}"
if [ ! -d ~/notes/.git ]; then
    echo -e "${RED}Notes repository not detected, initializing...${NC}"
    cd ~/notes
    git init
    echo "# Abby's Notes System" > README.md
    git add README.md
    git commit -m "Initializing notes repository"
    echo -e "${GREEN}Notes repository initialized.${NC}"
    echo -e "${BLUE}Please manually set up remote repository:${NC}"
    echo -e "git remote add origin https://github.com/drsc777/notes.git${NC}"
else
    echo -e "${GREEN}Notes repository already exists.${NC}"
fi

# Install Doom Emacs
echo -e "${BLUE}Checking Doom Emacs...${NC}"
if [ ! -d ~/.emacs.d ]; then
    echo -e "${RED}Doom Emacs not detected, installing...${NC}"
    git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
    ~/.emacs.d/bin/doom install
else
    echo -e "${GREEN}Doom Emacs already installed.${NC}"
fi

# Install Tmux plugin manager
echo -e "${BLUE}Checking Tmux plugin manager...${NC}"
if [ ! -d ~/.tmux/plugins/tpm ]; then
    echo -e "${RED}Tmux plugin manager not detected, installing...${NC}"
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
else
    echo -e "${GREEN}Tmux plugin manager already installed.${NC}"
fi

# Copy configuration files
echo -e "${BLUE}Copying configuration files...${NC}"
cp -r doom/* ~/.doom.d/
echo -e "${GREEN}Doom Emacs configuration copied.${NC}"

cp -r nvim/* ~/.config/nvim/
echo -e "${GREEN}NeoVim configuration copied.${NC}"

cp tmux/.tmux.conf ~/.tmux.conf
echo -e "${GREEN}Tmux configuration copied.${NC}"

cp .aerospace.toml ~/.config/aerospace/config.toml
echo -e "${GREEN}Aerospace configuration copied.${NC}"

# Copy Karabiner configuration
cp -r karabiner/* ~/.config/karabiner/
echo -e "${GREEN}Karabiner-Elements configuration copied.${NC}"

cp bin/* ~/bin/
chmod +x ~/bin/*.sh
echo -e "${GREEN}Utility scripts copied.${NC}"

# Set up auto-sync
echo -e "${BLUE}Setting up auto-sync...${NC}"
(crontab -l 2>/dev/null | grep -v sync-notes.sh; echo "*/15 * * * * ~/bin/sync-notes.sh >> ~/sync-notes.log 2>&1") | crontab -

# Add aliases to shell configuration
echo -e "${BLUE}Adding aliases to shell configuration...${NC}"
if [ -f ~/.zshrc ]; then
    SHELL_RC=~/.zshrc
elif [ -f ~/.bashrc ]; then
    SHELL_RC=~/.bashrc
else
    SHELL_RC=~/.zshrc
    touch $SHELL_RC
fi

# Check if aliases have already been added
if ! grep -q "alias sync-cursor" $SHELL_RC; then
    echo '' >> $SHELL_RC
    echo '# Custom commands' >> $SHELL_RC
    echo 'alias sync-cursor="~/bin/sync-cursor-nvim.sh"' >> $SHELL_RC
    echo 'alias sync-notes="~/bin/sync-notes.sh"' >> $SHELL_RC
    echo '' >> $SHELL_RC
    echo '# Ensure bin directory is in PATH' >> $SHELL_RC
    echo 'export PATH="$HOME/bin:$PATH"' >> $SHELL_RC
    echo -e "${GREEN}Aliases added to $SHELL_RC.${NC}"
else
    echo -e "${GREEN}Aliases already exist in $SHELL_RC.${NC}"
fi

# Sync Doom Emacs configuration
echo -e "${BLUE}Syncing Doom Emacs configuration...${NC}"
~/.emacs.d/bin/doom sync

# Install NeoVim plugins
echo -e "${BLUE}Installing NeoVim plugins...${NC}"
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync' || true

echo -e "${GREEN}Installation complete!${NC}"
echo -e "${BLUE}Notes:${NC}"
echo -e "1. In a new tmux session, press ${GREEN}Ctrl+a${NC} then ${GREEN}I${NC} to install tmux plugins"
echo -e "2. To auto-sync notes to GitHub, manually set up the remote repository:"
echo -e "   ${GREEN}cd ~/notes${NC}"
echo -e "   ${GREEN}git remote add origin https://github.com/drsc777/notes.git${NC}"
echo -e "   ${GREEN}git push -u origin main${NC}"
echo -e "3. Check ${GREEN}docs/keyboard-shortcuts.md${NC} to learn all keyboard shortcuts"
echo -e ""
echo -e "It's recommended to restart your terminal to apply all changes" 