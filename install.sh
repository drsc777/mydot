#!/bin/bash

# 一键安装脚本
# 用法: ./install.sh

# 设置颜色
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # 重置颜色

echo -e "${BLUE}开始安装无鼠标开发环境配置...${NC}"

# 检查依赖
echo -e "${BLUE}检查依赖...${NC}"
if ! command -v brew &> /dev/null; then
    echo -e "${RED}未检测到Homebrew，正在安装...${NC}"
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
else
    echo -e "${GREEN}Homebrew已安装.${NC}"
fi

# 安装必要的包
echo -e "${BLUE}安装必要的工具...${NC}"
brew install git emacs neovim tmux ripgrep fd fswatch || true

# 创建必要的目录
echo -e "${BLUE}创建必要的目录...${NC}"
mkdir -p ~/.doom.d
mkdir -p ~/.config/nvim/lua
mkdir -p ~/.config/aerospace
mkdir -p ~/bin
mkdir -p ~/notes/{org,roam,journal}

# 初始化笔记仓库
echo -e "${BLUE}初始化笔记仓库...${NC}"
if [ ! -d ~/notes/.git ]; then
    cd ~/notes
    git init
    echo "# 我的笔记系统" > README.md
    git add README.md
    git commit -m "初始化笔记仓库"
    echo -e "${GREEN}笔记仓库已初始化.${NC}"
else
    echo -e "${GREEN}笔记仓库已存在.${NC}"
fi

# 安装Doom Emacs
echo -e "${BLUE}检查Doom Emacs...${NC}"
if [ ! -d ~/.emacs.d ]; then
    echo -e "${RED}未检测到Doom Emacs，正在安装...${NC}"
    git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
    ~/.emacs.d/bin/doom install
else
    echo -e "${GREEN}Doom Emacs已安装.${NC}"
fi

# 安装Tmux插件管理器
echo -e "${BLUE}检查Tmux插件管理器...${NC}"
if [ ! -d ~/.tmux/plugins/tpm ]; then
    echo -e "${RED}未检测到Tmux插件管理器，正在安装...${NC}"
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
else
    echo -e "${GREEN}Tmux插件管理器已安装.${NC}"
fi

# 复制配置文件
echo -e "${BLUE}复制配置文件...${NC}"
cp -r doom/* ~/.doom.d/
echo -e "${GREEN}Doom Emacs配置已复制.${NC}"

cp -r nvim/* ~/.config/nvim/
echo -e "${GREEN}NeoVim配置已复制.${NC}"

cp tmux/.tmux.conf ~/.tmux.conf
echo -e "${GREEN}Tmux配置已复制.${NC}"

cp config.toml ~/.config/aerospace/
echo -e "${GREEN}Aerospace配置已复制.${NC}"

cp bin/* ~/bin/
chmod +x ~/bin/*.sh
echo -e "${GREEN}实用脚本已复制.${NC}"

# 设置自动同步
echo -e "${BLUE}设置自动同步...${NC}"
(crontab -l 2>/dev/null | grep -v sync-notes.sh; echo "*/15 * * * * ~/bin/sync-notes.sh >> ~/sync-notes.log 2>&1") | crontab -

# 添加别名到shell配置
echo -e "${BLUE}添加别名到shell配置...${NC}"
if [ -f ~/.zshrc ]; then
    SHELL_RC=~/.zshrc
elif [ -f ~/.bashrc ]; then
    SHELL_RC=~/.bashrc
else
    SHELL_RC=~/.zshrc
    touch $SHELL_RC
fi

# 检查是否已经添加了别名
if ! grep -q "alias sync-cursor" $SHELL_RC; then
    echo '' >> $SHELL_RC
    echo '# 自定义快捷命令' >> $SHELL_RC
    echo 'alias sync-cursor="~/bin/sync-cursor-nvim.sh"' >> $SHELL_RC
    echo 'alias sync-notes="~/bin/sync-notes.sh"' >> $SHELL_RC
    echo '' >> $SHELL_RC
    echo '# 确保bin目录在PATH中' >> $SHELL_RC
    echo 'export PATH="$HOME/bin:$PATH"' >> $SHELL_RC
    echo -e "${GREEN}别名已添加到$SHELL_RC.${NC}"
else
    echo -e "${GREEN}别名已存在于$SHELL_RC.${NC}"
fi

# 同步Doom Emacs配置
echo -e "${BLUE}同步Doom Emacs配置...${NC}"
~/.emacs.d/bin/doom sync

# 安装NeoVim插件
echo -e "${BLUE}安装NeoVim插件...${NC}"
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync' || true

echo -e "${GREEN}安装完成!${NC}"
echo -e "${BLUE}备注:${NC}"
echo -e "1. 请在新的tmux会话中按 ${GREEN}Ctrl+a${NC} 然后按 ${GREEN}I${NC} 来安装tmux插件"
echo -e "2. 如需自动同步笔记到GitHub, 请手动设置远程仓库:"
echo -e "   ${GREEN}cd ~/notes${NC}"
echo -e "   ${GREEN}git remote add origin https://github.com/drsc777/mydot.git${NC}"
echo -e "   ${GREEN}git push -u origin main${NC}"
echo -e "3. 请查看${GREEN}docs/keyboard-shortcuts.md${NC}了解所有快捷键"
echo -e ""
echo -e "建议重启一下终端来应用所有变更" 