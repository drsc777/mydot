#!/bin/bash

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # Reset color

echo -e "${BLUE}Setting up key repeat for improved Vim experience...${NC}"

# Key repeat settings for macOS
echo -e "${BLUE}Adjusting key repeat settings for the entire system...${NC}"
# Set a shorter delay until repeat
defaults write NSGlobalDomain InitialKeyRepeat -int 15
# Set a faster repeat rate
defaults write NSGlobalDomain KeyRepeat -int 2

# Settings specific for Cursor app
echo -e "${BLUE}Configuring Cursor app for Vim mode...${NC}"

# Create config directory if it doesn't exist
CURSOR_CONFIG_DIR="$HOME/Library/Application Support/Cursor/User"
mkdir -p "$CURSOR_CONFIG_DIR"

# Copy Vim settings
echo -e "${BLUE}Copying Vim settings for Cursor...${NC}"
cp ~/mydot/cursor/cursor_vim_settings.json "$CURSOR_CONFIG_DIR/settings.json"

# Apply Cursor-specific key repeat settings
echo -e "${BLUE}Setting up key repeat specifically for Cursor...${NC}"
# This disables the character popover for macOS
defaults write com.cursor.Cursor ApplePressAndHoldEnabled -bool false

echo -e "${GREEN}Key repeat settings applied!${NC}"
echo -e "${GREEN}Please restart your computer for the changes to take effect.${NC}"
echo -e "${BLUE}Notes:${NC}"
echo -e "1. You may need to restart Cursor app if it's currently running."
echo -e "2. If you want to revert these changes, run:"
echo -e "${GREEN}   defaults delete NSGlobalDomain InitialKeyRepeat${NC}"
echo -e "${GREEN}   defaults delete NSGlobalDomain KeyRepeat${NC}"
echo -e "${GREEN}   defaults delete com.cursor.Cursor ApplePressAndHoldEnabled${NC}" 