#!/bin/bash

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # Reset color

echo -e "${BLUE}=== Alternative XVim2 Installation for Xcode 16.2+ ===${NC}"

# 确保Xcode已安装
if [ ! -d "/Applications/Xcode.app" ]; then
    echo -e "${RED}Xcode not found in /Applications/Xcode.app${NC}"
    exit 1
fi

# 获取Xcode版本
XCODE_VERSION=$(xcodebuild -version | grep Xcode | cut -d ' ' -f 2)
echo -e "${BLUE}Detected Xcode version: ${XCODE_VERSION}${NC}"

# 彻底清理
echo -e "${BLUE}Performing thorough cleanup...${NC}"
rm -rf ~/xvim_temp
rm -rf ~/Library/Application\ Support/Developer/Shared/Xcode/Plug-ins/XVim2.xcplugin
defaults delete com.apple.dt.Xcode DVTPlugInManagerNonApplePlugIns-Xcode-${XCODE_VERSION} 2>/dev/null
defaults delete com.apple.dt.Xcode DVTPlugInManagerNonApplePlugIns-Xcode-* 2>/dev/null

# 确认用户准备好重新安装
echo -e "${YELLOW}This script will now:${NC}"
echo -e "1. Clone XVim2 repository"
echo -e "2. Apply compatibility fixes for Xcode 16.2+"
echo -e "3. Create a code signing certificate"
echo -e "4. Sign Xcode (requires sudo access)"
echo -e "5. Build and install XVim2"
echo -e "${YELLOW}The process will require your password for sudo operations.${NC}"
echo -e "${YELLOW}Press ENTER to continue or CTRL+C to abort...${NC}"
read

# 确保xvimrc配置文件存在
cp ~/Desktop/mydot/xvim/xvimrc ~/.xvimrc
echo -e "${GREEN}✓ Copied .xvimrc to home directory${NC}"

# 创建临时目录
echo -e "${BLUE}Creating temporary directory...${NC}"
mkdir -p ~/xvim_temp
cd ~/xvim_temp

# 克隆XVim2仓库
echo -e "${BLUE}Cloning XVim2 repository...${NC}"
git clone https://github.com/XVimProject/XVim2.git
cd XVim2

# 检查是否需要应用补丁以支持Xcode 16.2+
echo -e "${BLUE}Checking compatibility with Xcode ${XCODE_VERSION}...${NC}"

# 创建和应用补丁来添加Xcode 16.2+的支持
if [[ $(echo "${XCODE_VERSION}" | cut -d. -f1) -ge 16 ]]; then
    echo -e "${YELLOW}Applying compatibility patch for Xcode 16.2+...${NC}"
    
    # 生成一个UUID
    NEW_UUID=$(uuidgen)
    echo -e "${YELLOW}Generated UUID: ${NEW_UUID}${NC}"
    
    # 修改XVim2的Info.plist模板
    PLIST_TEMPLATE="XVim2/Info.plist"
    if [ -f "$PLIST_TEMPLATE" ]; then
        echo -e "${BLUE}Modifying XVim2 plugin info template...${NC}"
        # 尝试添加UUID到DVTPlugInCompatibilityUUIDs数组
        /usr/libexec/PlistBuddy -c "Add :DVTPlugInCompatibilityUUIDs: array" "$PLIST_TEMPLATE" 2>/dev/null
        /usr/libexec/PlistBuddy -c "Add :DVTPlugInCompatibilityUUIDs:0 string $NEW_UUID" "$PLIST_TEMPLATE" 2>/dev/null
    else
        echo -e "${RED}Could not find $PLIST_TEMPLATE. Plugin may not work.${NC}"
    fi
fi

# 创建证书
echo -e "${BLUE}Setting up code signing certificate...${NC}"

# 检查是否已有XcodeSigner证书
if security find-certificate -c "XcodeSigner" ~/Library/Keychains/login.keychain-db > /dev/null 2>&1; then
    echo -e "${GREEN}Certificate 'XcodeSigner' already exists.${NC}"
else
    echo -e "${YELLOW}Creating certificate...${NC}"
    
    # 先尝试命令行方式创建证书
    echo -e "${BLUE}Attempting to create certificate via command line...${NC}"
    security create-certificate -k ~/Library/Keychains/login.keychain-db -T /usr/bin/codesign -s "XcodeSigner" -c R "XcodeSigner" -n "XcodeSigner" -b
    
    # 检查是否创建成功
    if ! security find-certificate -c "XcodeSigner" ~/Library/Keychains/login.keychain-db > /dev/null 2>&1; then
        echo -e "${RED}Failed to create certificate via command line.${NC}"
        echo -e "${YELLOW}Please create the certificate manually:${NC}"
        echo "1. Open Keychain Access"
        echo "2. Go to Keychain Access > Certificate Assistant > Create a Certificate"
        echo "3. Set name to 'XcodeSigner', Identity Type to 'Self Signed Root'"
        echo "4. Set Certificate Type to 'Code Signing'"
        echo "5. Check 'Let me override defaults'"
        echo "6. Continue through the wizard"
        echo -e "${YELLOW}Press ENTER when you've created the certificate...${NC}"
        read
    fi
fi

# 签名Xcode
echo -e "${BLUE}Signing Xcode (requires sudo password)...${NC}"
sudo codesign -f -s XcodeSigner /Applications/Xcode.app
if [ $? -ne 0 ]; then
    echo -e "${RED}Failed to sign Xcode. XVim2 may not work.${NC}"
else
    echo -e "${GREEN}✓ Xcode signed successfully${NC}"
fi

# 使用Xcode UUID修改环境变量
echo -e "${BLUE}Setting up environment with UUID...${NC}"
export XCODE_PLUGIN_UUID="$NEW_UUID"

# 构建和安装XVim2
echo -e "${BLUE}Building and installing XVim2...${NC}"
make clean
make
if [ $? -ne 0 ]; then
    echo -e "${RED}XVim2 build failed. See above for errors.${NC}"
else
    echo -e "${GREEN}✓ XVim2 built and installed successfully${NC}"
fi

# 清理
echo -e "${BLUE}Cleaning up...${NC}"
cd ~
rm -rf ~/xvim_temp

# 强制Xcode加载插件的设置
echo -e "${BLUE}Setting up Xcode to load XVim2...${NC}"
defaults write com.apple.dt.Xcode DVTPlugInManagerNonApplePlugIns-Xcode-${XCODE_VERSION} -bool YES

echo -e "${GREEN}=== XVim2 installation completed! ===${NC}"
echo -e "${YELLOW}IMPORTANT: You MUST complete these steps:${NC}"
echo -e "1. Completely quit Xcode if it's running"
echo -e "2. Open Terminal and run: ${BLUE}sudo xattr -cr /Applications/Xcode.app${NC}"
echo -e "3. Restart your Mac"
echo -e "4. When you start Xcode again, if asked about XVim2 plugin, select 'Load Bundle'"
echo -e ""
echo -e "${BLUE}If XVim2 still doesn't work after restart:${NC}"
echo -e "1. Open Xcode and check ${YELLOW}Xcode menu > Settings > Extensions${NC} for XVim2"
echo -e "2. Verify if Vim commands work (try pressing 'j', 'k', ':', etc.)"
echo -e ""
echo -e "${RED}NOTE: XVim2 may not be fully compatible with latest Xcode versions.${NC}"
echo -e "${RED}As an alternative, consider using BBEdit with the 'bbedit --wait' command for Vim-like editing.${NC}" 