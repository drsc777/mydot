# 无鼠标开发环境完全新手指南

欢迎阅读这份无鼠标开发环境的完整新手指南。本教程将详细介绍如何安装和配置本仓库中的所有工具，并提供清晰的步骤说明。

## 目录

1. [简介](#简介)
2. [前提条件](#前提条件)
3. [安装](#安装)
4. [了解你的新环境](#了解你的新环境)
5. [Doom Emacs基础](#doom-emacs基础)
6. [Neovim基础](#neovim基础)
7. [Tmux基础](#tmux基础)
8. [Aerospace：窗口管理](#aerospace窗口管理)
9. [Karabiner-Elements：键盘定制](#karabiner-elements键盘定制)
10. [Raycast：启动器和工具集](#raycast启动器和工具集)
11. [常见工作流程](#常见工作流程)
12. [故障排除](#故障排除)
13. [进一步资源](#进一步资源)

## 简介

这个环境创建了一个"无鼠标"的开发设置，专注于通过键盘快捷键最大化生产力。主要组件包括：

- **Doom Emacs**：具有模态编辑功能的Emacs配置框架
- **Neovim**：Vim的现代改进版本
- **Tmux**：终端复用器，用于管理多个终端会话
- **Aerospace**：macOS窗口管理器
- **Karabiner-Elements**：用于重新映射键位和创建复杂修改的工具
- **Raycast**：替代Spotlight搜索的生产力工具

学完本教程后，你将拥有一个完全配置好的开发环境，并了解如何有效地使用每个组件。

## 前提条件

开始前，请确保你有：

- 运行macOS的Mac电脑（建议Big Sur或更新版本）
- 安装软件的管理员权限
- 基本的终端熟悉度（知道如何打开Terminal.app就足够了）
- 至少5GB的可用磁盘空间
- 稳定的互联网连接
- 1-2小时的时间完成设置

## 安装

### 自动安装

对于快速设置，你可以使用自动安装脚本：

1. 打开终端（按下Cmd+Space，输入"Terminal"，按Enter）
2. 运行以下命令：

```bash
bash -c "$(curl -fsSL https://raw.githubusercontent.com/drsc777/mydot/main/install.sh)"
```

3. 按照屏幕上的提示操作
4. 完成后，重启终端

### 手动安装

如果你想了解每个步骤或自动安装不起作用，请按照以下步骤操作：

#### 1. 安装Homebrew

Homebrew是macOS的包管理器，可以轻松安装软件：

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

安装后，按照指示将Homebrew添加到你的PATH中。

#### 2. 安装核心依赖

```bash
# 安装命令行工具
brew install git emacs neovim tmux ripgrep fd fswatch

# 安装GUI应用
brew install --cask karabiner-elements raycast
```

#### 3. 克隆配置仓库

```bash
# 为dotfiles创建临时目录
mkdir -p ~/Desktop/mydot
cd ~/Desktop/mydot

# 克隆仓库
git clone https://github.com/drsc777/mydot.git .
```

#### 4. 设置Doom Emacs

```bash
# 克隆Doom Emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d

# 安装Doom
~/.emacs.d/bin/doom install

# 复制Doom配置
mkdir -p ~/.doom.d
cp -r ~/Desktop/mydot/doom/* ~/.doom.d/

# 同步Doom配置
~/.emacs.d/bin/doom sync
```

#### 5. 设置Neovim

```bash
# 创建Neovim配置目录
mkdir -p ~/.config/nvim/lua

# 复制Neovim配置
cp -r ~/Desktop/mydot/nvim/* ~/.config/nvim/

# 安装Neovim插件（这可能需要几分钟）
nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'
```

#### 6. 设置Tmux

```bash
# 安装Tmux插件管理器
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm

# 复制Tmux配置
cp ~/Desktop/mydot/tmux/.tmux.conf ~/.tmux.conf
```

#### 7. 设置Aerospace（窗口管理器）

```bash
# 创建配置目录
mkdir -p ~/.config/aerospace

# 复制配置
cp ~/Desktop/mydot/aerospace/.aerospace.toml ~/.config/aerospace/config.toml
```

#### 8. 设置实用脚本

```bash
# 创建bin目录
mkdir -p ~/bin

# 复制实用脚本
cp ~/Desktop/mydot/bin/* ~/bin/
chmod +x ~/bin/*.sh
```

#### 9. 配置Shell集成

将以下内容添加到你的~/.zshrc或~/.bashrc：

```bash
# 自定义别名
alias sync-cursor="~/bin/sync-cursor-nvim.sh"
alias sync-notes="~/bin/sync-notes.sh"

# 确保bin目录在PATH中
export PATH="$HOME/bin:$PATH"
```

#### 10. 设置笔记系统

```bash
mkdir -p ~/notes/{org,roam,journal}
cd ~/notes
git init
echo "# 我的笔记系统" > README.md
git add README.md
git commit -m "初始化笔记仓库"
```

## 了解你的新环境

### 系统组件概述

你的新开发环境由以下集成组件组成：

1. **终端和Shell**：大多数工具运行的基础
2. **Tmux**：在一个窗口中创建多个终端会话
3. **Doom Emacs**：具有类似Vim键绑定和IDE功能的文本编辑器
4. **Neovim**：用于快速编辑的轻量级代码编辑器
5. **Aerospace**：控制窗口位置和工作区
6. **Karabiner-Elements**：自定义键盘行为
7. **Raycast**：提供对命令和应用程序的快速访问

### 配置文件位置

所有配置文件（"dotfiles"）存储在特定位置：

- Doom Emacs：`~/.doom.d/`
- Neovim：`~/.config/nvim/`
- Tmux：`~/.tmux.conf`
- Aerospace：`~/.config/aerospace/config.toml`
- Karabiner：`~/.config/karabiner/`
- 实用脚本：`~/bin/`

## Doom Emacs基础

### 启动Doom Emacs

要启动Doom Emacs，打开终端并输入：

```bash
emacs
```

### 关键概念

- **模态编辑**：像Vim一样，Doom有不同的模式（普通、插入、可视）
- **Leader键**：空格是"leader键"，触发大多数命令
- **Which-key**：显示可用命令的菜单

### 基本命令

- `ESC`或`Ctrl+g`：取消/返回普通模式
- `i`：进入插入模式（用于输入）
- `Space .`：查找文件
- `Space ,`：切换缓冲区
- `Space :`：运行命令（类似M-x）
- `Space f s`：保存文件
- `Space q q`：退出Emacs

### Org Mode基础

Org mode是一个强大的笔记和组织系统：

- 使用`*`创建新标题（更多星号表示子标题）
- 在标题上按`Tab`：循环折叠状态
- `Shift+Tab`：循环所有标题
- `Ctrl+Enter`：创建新标题
- `Alt+Enter`：在当前级别创建新项目
- `Space m a`：添加动作/待办项

### Org Roam基础

Org Roam创建一个链接笔记网络：

- `Space n r f`：查找或创建笔记
- `Space n r i`：插入到另一个笔记的链接
- `Space n r b`：显示反向链接缓冲区

## Neovim基础

### 启动Neovim

要启动Neovim，打开终端并输入：

```bash
nvim
```

### 关键概念

Neovim使用与Vim相同的模态编辑范式：

- **普通模式**：用于导航和命令
- **插入模式**：用于输入文本
- **可视模式**：用于选择文本
- **命令模式**：用于输入带有`:`前缀的命令

### 基本命令

- `i`：进入插入模式
- `Esc`：返回普通模式
- `:w`：保存文件
- `:q`：退出（`:wq`保存并退出）
- `/搜索词`：向前搜索
- `n`：查找下一个匹配项
- `Space e`：切换文件浏览器
- `Space ff`：查找文件
- `Space fg`：实时grep搜索
- `Ctrl+h/j/k/l`：在分割窗口间导航

## Tmux基础

### 启动Tmux

要启动新的Tmux会话：

```bash
tmux
```

### 关键概念

- **前缀键**：`Ctrl+a`是前缀（必须在其他命令之前按下）
- **会话**：独立的终端环境
- **窗口**：会话中的标签
- **窗格**：窗口内的分割区域

### 基本命令

- `Ctrl+a c`：创建新窗口
- `Ctrl+a ,`：重命名当前窗口
- `Ctrl+a n`：下一个窗口
- `Ctrl+a p`：上一个窗口
- `Ctrl+a %`：垂直分割
- `Ctrl+a "`：水平分割
- `Ctrl+a 箭头键`：导航窗格
- `Ctrl+a d`：从会话分离
- `tmux attach`：重新连接会话

## Aerospace：窗口管理

### 关键概念

Aerospace自动管理屏幕上的窗口位置。

### 基本命令

- `Option+Enter`：切换全屏
- `Option+h/j/k/l`：在窗口间移动焦点
- `Option+Shift+h/j/k/l`：移动窗口
- `Option+1-9`：切换到工作区
- `Option+Shift+1-9`：将窗口移动到工作区

## Karabiner-Elements：键盘定制

### 关键概念

Karabiner重新映射键位以提高人体工学和效率。

### 重要修改

- Caps Lock轻点时作为Escape，按住时作为Control
- 右Option成为Hyper键（Ctrl+Shift+Cmd+Option）

## Raycast：启动器和工具集

### 关键概念

Raycast用增强功能取代Spotlight。

### 基本命令

- `Option+Space`：打开Raycast
- 开始输入以搜索应用程序、文件或操作
- `Option+->`：查看找到项目的扩展和操作

## 常见工作流程

### 写作和笔记

1. 打开Emacs（`emacs`）
2. 访问Org Roam（`Space n r f`）
3. 输入笔记名称或创建新笔记
4. 使用Org Mode格式写内容
5. 使用`Space f s`保存

### 编码会话

1. 启动Tmux（`tmux`）
2. 为代码创建窗口（`Ctrl+a c`）
3. 打开Neovim（`nvim project-file.py`）
4. 为终端创建另一个窗口（`Ctrl+a c`）
5. 使用`Ctrl+a n`和`Ctrl+a p`在窗口间切换
6. 在Neovim中编辑代码，在终端中运行

### 窗口管理

1. 打开你任务需要的应用程序
2. 使用`Option+h/j/k/l`在窗口间导航
3. 使用`Option+Shift+h/j/k/l`安排它们
4. 使用`Option+1-9`组织到工作区

## 故障排除

### 常见问题和解决方案

1. **Emacs显示奇怪字符**
   - 确保你已安装兼容的字体：`brew install --cask font-fira-code`

2. **Neovim插件不工作**
   - 在Neovim中运行`:checkhealth`诊断
   - 确保已安装Node.js：`brew install node`

3. **Tmux显示"invalid prefix key"错误**
   - 确保`.tmux.conf`正确复制到你的主目录

4. **Karabiner不工作**
   - 确保它在系统偏好设置中有辅助功能权限

### 获取帮助

如果你遇到问题：

1. 查看README.md的故障排除部分
2. 查找错误消息并在线搜索
3. 查看特定工具的文档
4. 在GitHub仓库上提出问题

## 进一步资源

### 学习资源

- Doom Emacs文档：https://github.com/doomemacs/doomemacs/blob/master/docs/index.org
- Neovim文档：https://neovim.io/doc/
- Tmux备忘单：https://tmuxcheatsheet.com/
- Org Mode手册：https://orgmode.org/manual/

### 社区支持

- Doom Emacs Discord：https://discord.gg/qvGgnVx
- Neovim论坛：https://neovim.discourse.group/
- Reddit社区：r/emacs、r/vim、r/neovim

---

请记住，掌握这个环境需要时间。专注于每天学习几个命令，而不是试图一次记住所有内容。通过练习，这些工具将成为第二天性，并显著提高你的生产力。 