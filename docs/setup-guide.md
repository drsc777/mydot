# 安装指南

本指南将帮助你设置与我相同的开发环境。安装过程分为几个部分，按照推荐的顺序进行安装。

## 预备知识

这套配置适合熟悉终端和命令行的用户，如果你是新手，可能需要额外学习一些基础知识。

## 必要依赖

首先，安装必要的依赖：

```bash
# 安装Homebrew (macOS包管理器)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# 安装必要工具
brew install git emacs ripgrep fd tmux fswatch

# 安装NeoVim
brew install neovim
```

## 1. 安装Doom Emacs

```bash
# 克隆Doom Emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d

# 安装Doom
~/.emacs.d/bin/doom install

# 创建配置目录
mkdir -p ~/.doom.d
```

## 2. 安装Tmux插件管理器

```bash
# 安装Tmux插件管理器
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

## 3. 准备NeoVim配置目录

```bash
# 创建NeoVim配置目录
mkdir -p ~/.config/nvim/lua
```

## 4. 准备Aerospace配置

```bash
# 创建Aerospace配置目录
mkdir -p ~/.config/aerospace
```

## 5. 创建脚本目录

```bash
# 创建bin目录
mkdir -p ~/bin
```

## 6. 安装我的配置

### 克隆配置仓库

```bash
# 克隆配置仓库
git clone https://github.com/yourusername/mydot.git ~/temp-dotfiles
```

或者，如果你已经下载了这个仓库，可以直接使用。

### 复制配置文件

```bash
# 复制Doom Emacs配置
cp -r ~/temp-dotfiles/doom/* ~/.doom.d/

# 复制NeoVim配置
cp -r ~/temp-dotfiles/nvim/* ~/.config/nvim/

# 复制Tmux配置
cp ~/temp-dotfiles/tmux/.tmux.conf ~/.tmux.conf

# 复制Aerospace配置
cp ~/temp-dotfiles/config.toml ~/.config/aerospace/

# 复制实用脚本
cp ~/temp-dotfiles/bin/* ~/bin/
chmod +x ~/bin/*.sh
```

## 7. 应用配置

### Doom Emacs

```bash
# 同步Doom配置
~/.emacs.d/bin/doom sync
```

### NeoVim

```bash
# 安装插件
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
```

### Tmux

在tmux中，按 `Ctrl+a` 然后按 `I` 来安装插件。

### 设置自动同步

```bash
# 添加crontab任务
(crontab -l 2>/dev/null; echo "*/15 * * * * ~/bin/sync-notes.sh >> ~/sync-notes.log 2>&1") | crontab -
```

### 设置Shell别名

将以下内容添加到你的 `~/.zshrc` 或 `~/.bashrc`：

```bash
# 自定义快捷命令
alias sync-cursor="~/bin/sync-cursor-nvim.sh"
alias sync-notes="~/bin/sync-notes.sh"

# 确保bin目录在PATH中
export PATH="$HOME/bin:$PATH"
```

## 8. 创建笔记目录

```bash
# 创建笔记目录结构
mkdir -p ~/notes/{org,roam,journal}
cd ~/notes
git init
echo "# 我的笔记系统" > README.md
```

## 9. 设置GitHub仓库（可选）

如果你想自动同步笔记到GitHub：

1. 在GitHub上创建一个新仓库（例如"notes"）
2. 添加远程链接：

```bash
cd ~/notes
git remote add origin https://github.com/yourusername/notes.git
git add .
git commit -m "Initial commit"
git push -u origin main
```

## 10. 安装字体（推荐）

为了获得最佳体验，建议安装一款编程字体：

```bash
brew tap homebrew/cask-fonts
brew install --cask font-fira-code
```

## 完成！

恭喜！你现在已经设置好了一个完整的无鼠标开发环境。请参考[快捷键指南](keyboard-shortcuts.md)学习如何使用这套工具。 