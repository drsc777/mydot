#!/bin/bash

# 设置笔记目录 | Set notes directory
NOTES_DIR=~/notes

# 如果目录不存在，创建它 | If directory doesn't exist, create it
if [ ! -d "$NOTES_DIR" ]; then
  echo "创建笔记目录: $NOTES_DIR | Creating notes directory: $NOTES_DIR"
  mkdir -p "$NOTES_DIR"
fi

cd "$NOTES_DIR"

# 如果不是git仓库，初始化它 | If not a git repository, initialize it
if [ ! -d ".git" ]; then
  echo "初始化Git仓库 | Initializing Git repository"
  git init
  echo "# 我的笔记 | My Notes" > README.md
  git add README.md
  git commit -m "初始化笔记仓库 | Initialize notes repository"
  
  echo "请手动设置远程仓库，例如: | Please manually set up remote repository, for example:"
  echo "git remote add origin https://github.com/你的用户名/notes.git | git remote add origin https://github.com/yourusername/notes.git"
  exit 0
fi

# 检查是否有更改 | Check if there are changes
if [[ -z $(git status -s) ]]; then
  echo "没有需要同步的变更 | No changes to sync"
  exit 0
fi

# 添加所有更改 | Add all changes
git add .

# 提交更改 | Commit changes
git commit -m "Auto-sync notes: $(date +'%Y-%m-%d %H:%M:%S')"

# 尝试推送到远程 | Try to push to remote
if git remote | grep -q origin; then
  git push origin main || git push origin master || echo "推送失败，可能需要设置远程仓库 | Push failed, may need to set up remote repository"
else
  echo "请先设置远程仓库，例如: | Please set up remote repository first, for example:"
  echo "git remote add origin https://github.com/你的用户名/notes.git | git remote add origin https://github.com/yourusername/notes.git"
fi 