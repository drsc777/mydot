#!/bin/bash

# 设置 kitty 为默认终端
defaults write com.apple.LaunchServices/com.apple.launchservices.secure LSHandlers -array-add '{"LSHandlerContentType" = "public.unix-executable"; "LSHandlerRoleAll" = "com.kovidgoyal.kitty";}'
defaults write com.apple.LaunchServices/com.apple.launchservices.secure LSHandlers -array-add '{"LSHandlerURLScheme" = "ssh"; "LSHandlerRoleAll" = "com.kovidgoyal.kitty";}'
defaults write com.apple.LaunchServices/com.apple.launchservices.secure LSHandlers -array-add '{"LSHandlerURLScheme" = "sftp"; "LSHandlerRoleAll" = "com.kovidgoyal.kitty";}'
defaults write com.apple.LaunchServices/com.apple.launchservices.secure LSHandlers -array-add '{"LSHandlerURLScheme" = "telnet"; "LSHandlerRoleAll" = "com.kovidgoyal.kitty";}'

# 刷新 LaunchServices 数据库
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user

echo "Kitty has been set as the default terminal. Please restart your computer for changes to take effect." 