#!/bin/bash

# 设置项目目录 | Set project directory
PROJECT_DIR=$1
if [ -z "$PROJECT_DIR" ]; then
  echo "请指定要监控的项目目录 | Please specify the project directory to monitor"
  exit 1
fi

# 清理旧进程 | Clean up old processes
pkill -f "fswatch -o $PROJECT_DIR"

# 启动双向同步 | Start bidirectional sync
echo "开始监控 $PROJECT_DIR 的文件变化... | Starting to monitor file changes in $PROJECT_DIR..."

# 监控NeoVim的更改并通知Cursor | Monitor NeoVim changes and notify Cursor
fswatch -o "$PROJECT_DIR" | xargs -n1 -I{} osascript -e 'tell application "Cursor" to activate' &

# 记录进程ID到日志文件 | Log process ID
echo "同步进程已启动，PID: $! | Sync process started, PID: $!" 