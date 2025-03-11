#!/bin/bash

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # Reset color

echo -e "${BLUE}=== Advanced Key Settings for Cursor Vim Mode ===${NC}"

# 设置极端的按键重复速率
echo -e "${BLUE}Setting extreme key repeat rate...${NC}"
# 设置最短的延迟
defaults write NSGlobalDomain InitialKeyRepeat -int 8
# 设置最快的重复速率
defaults write NSGlobalDomain KeyRepeat -int 1

# 关闭所有应用的按键长按弹出菜单
echo -e "${BLUE}Disabling press-and-hold for all applications...${NC}"
defaults write NSGlobalDomain ApplePressAndHoldEnabled -bool false

# Cursor特定设置
echo -e "${BLUE}Applying Cursor-specific settings...${NC}"
defaults write com.cursor.Cursor ApplePressAndHoldEnabled -bool false
defaults write com.cursor.Cursor KeyRepeat -int 1
defaults write com.cursor.Cursor InitialKeyRepeat -int 8
defaults write com.cursor.Cursor AppleKeyboardUIMode -int 3

# VSCode/Cursor配置目录
CURSOR_CONFIG_DIR="$HOME/Library/Application Support/Cursor/User"
mkdir -p "$CURSOR_CONFIG_DIR"

# 创建最优化的VSCodeVim配置
echo -e "${BLUE}Creating optimized VSCodeVim configuration...${NC}"
cat > "$CURSOR_CONFIG_DIR/settings.json" << EOF
{
  "vim.leader": "<space>",
  "vim.hlsearch": true,
  "vim.incsearch": true,
  "vim.useSystemClipboard": true,
  "vim.useCtrlKeys": true,
  "vim.timeout": 200,
  "vim.insertModeKeyBindings": [
    {
      "before": ["j", "j"],
      "after": ["<Esc>"]
    }
  ],
  "vim.normalModeKeyBindingsNonRecursive": [
    {
      "before": ["<leader>", "w"],
      "commands": [":w"]
    },
    {
      "before": ["<leader>", "q"],
      "commands": [":q"]
    },
    {
      "before": ["<leader>", "e"],
      "commands": ["workbench.view.explorer"]
    },
    {
      "before": ["<leader>", "f"],
      "commands": ["workbench.action.quickOpen"]
    },
    {
      "before": ["<leader>", "g"],
      "commands": ["workbench.action.findInFiles"]
    },
    {
      "before": [";"],
      "after": [":"]
    }
  ],
  "vim.handleKeys": {
    "<C-a>": false,
    "<C-f>": false,
    "<C-c>": false,
    "<C-v>": false,
    "<C-x>": false,
    "<C-z>": false
  },
  "editor.cursorBlinking": "solid",
  "vim.easymotion": true,
  "vim.sneak": true,
  "vim.surround": true,
  "keyboard.dispatch": "keyCode",
  "editor.autoClosingBrackets": "never",
  "editor.autoClosingQuotes": "never",
  "editor.acceptSuggestionOnEnter": "off",
  "vim.foldfix": true,
  "vim.enableNeovim": true,
  "editor.lineNumbers": "relative",
  "vim.smartRelativeLine": true,
  "vim.overrideCopy": false,
  "vim.camelCaseMotion.enable": true,
  "vim.visualstar": true,
  "vim.cursorStylePerMode.insert": "line",
  "vim.cursorStylePerMode.normal": "block",
  "vim.cursorStylePerMode.replace": "underline",
  "vim.cursorStylePerMode.visual": "block-outline",
  "vim.cursorStylePerMode.visualblock": "block-outline",
  "vim.cursorStylePerMode.visualline": "block-outline",
  "editor.fontLigatures": false
}
EOF

# 重启一些系统服务
echo -e "${BLUE}Restarting services to apply changes...${NC}"
killall Dock
killall Finder
killall SystemUIServer

echo -e "${GREEN}=== Advanced key settings applied! ===${NC}"
echo -e "${YELLOW}Important: Please restart your computer AND Cursor app${NC}"
echo -e "${BLUE}After restart, try these tests in Cursor:${NC}"
echo -e "1. Long-press 'j' key in normal mode - should move down continuously"
echo -e "2. Type ':' to enter command mode"
echo -e "3. Try hjkl navigation in normal mode"
echo -e ""
echo -e "${BLUE}If still not working, try:${NC}"
echo -e "1. In macOS Settings > Keyboard > Input Sources, disable any additional input methods"
echo -e "2. In macOS Settings > Keyboard, set Key Repeat to 'Fast' and Delay Until Repeat to 'Short'" 