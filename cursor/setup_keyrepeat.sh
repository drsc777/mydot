#!/bin/bash

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # Reset color

echo -e "${BLUE}Setting up key repeat for improved Vim experience...${NC}"

# Key repeat settings for macOS - setting to ultra fast
echo -e "${BLUE}Adjusting key repeat settings for the entire system...${NC}"
# Set a very short delay until repeat
defaults write NSGlobalDomain InitialKeyRepeat -int 10
# Set a very fast repeat rate
defaults write NSGlobalDomain KeyRepeat -int 1
# Disable press-and-hold for all apps
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Additional system settings to improve responsiveness
echo -e "${BLUE}Applying additional system settings to improve responsiveness...${NC}"
# Reduce UI animations
defaults write NSGlobalDomain NSAutomaticWindowAnimationsEnabled -bool false
# Faster dock animations
defaults write com.apple.dock autohide-time-modifier -float 0.15
# Faster Mission Control animations
defaults write com.apple.dock expose-animation-duration -float 0.15

# Settings specific for Cursor app
echo -e "${BLUE}Configuring Cursor app for Vim mode...${NC}"

# Create config directory if it doesn't exist
CURSOR_CONFIG_DIR="$HOME/Library/Application Support/Cursor/User"
mkdir -p "$CURSOR_CONFIG_DIR"

# Copy Vim settings
echo -e "${BLUE}Copying Vim settings for Cursor...${NC}"
cp ~/Desktop/mydot/cursor/cursor_vim_settings.json "$CURSOR_CONFIG_DIR/settings.json"

# Apply Cursor-specific key repeat settings
echo -e "${BLUE}Setting up key repeat specifically for Cursor...${NC}"
# This disables the character popover for Cursor app
defaults write com.cursor.Cursor ApplePressAndHoldEnabled -bool false
# Force keyboard type to be direct dispatch
defaults write com.cursor.Cursor AppleKeyboardUIMode -int 3

echo -e "${GREEN}Key repeat settings applied!${NC}"
echo -e "${BLUE}Restarting Dock and Finder to apply some settings...${NC}"
killall Dock
killall Finder

echo -e "${GREEN}Please restart your computer for all changes to take full effect.${NC}"
echo -e "${BLUE}Notes:${NC}"
echo -e "1. You need to restart Cursor app if it's currently running."
echo -e "2. If you want to revert these changes, run:"
echo -e "${GREEN}   defaults delete NSGlobalDomain InitialKeyRepeat${NC}"
echo -e "${GREEN}   defaults delete NSGlobalDomain KeyRepeat${NC}"
echo -e "${GREEN}   defaults delete NSGlobalDomain ApplePressAndHoldEnabled${NC}"
echo -e "${GREEN}   defaults delete com.cursor.Cursor ApplePressAndHoldEnabled${NC}" 