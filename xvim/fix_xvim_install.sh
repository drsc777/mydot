#!/bin/bash

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[0;33m'
NC='\033[0m' # Reset color

echo -e "${BLUE}=== Fixed XVim2 Installation for Xcode 16.2 ===${NC}"

# 确保xvimrc配置文件存在
cp ~/Desktop/mydot/xvim/xvimrc ~/.xvimrc
echo -e "${GREEN}✓ Copied .xvimrc to home directory${NC}"

# 清理旧的临时文件和目录
echo -e "${BLUE}Cleaning up any previous installation...${NC}"
rm -rf ~/xvim_temp
rm -f ~/create_certificate.sh

# 创建临时目录
mkdir -p ~/xvim_temp
cd ~/xvim_temp

# 克隆XVim2仓库
echo -e "${BLUE}Cloning XVim2 repository...${NC}"
git clone https://github.com/XVimProject/XVim2.git
cd XVim2

# 检查Xcode版本
XCODE_VERSION=$(xcodebuild -version | grep Xcode | cut -d ' ' -f 2)
echo -e "${BLUE}Detected Xcode version: ${XCODE_VERSION}${NC}"

# 手动获取和添加UUID
echo -e "${YELLOW}Manually setting up Xcode UUID compatibility...${NC}"
XCODE_PLIST_PATH="/Applications/Xcode.app/Contents/Info.plist"
if [ -f "$XCODE_PLIST_PATH" ]; then
    # 使用PlistBuddy提取UUID，如果存在的话
    UUID=$(/usr/libexec/PlistBuddy -c "Print :DVTPlugInCompatibilityUUID" "$XCODE_PLIST_PATH" 2>/dev/null)
    
    if [ -z "$UUID" ]; then
        echo -e "${YELLOW}UUID not found in Info.plist, generating a new one...${NC}"
        # 生成一个随机UUID作为替代
        UUID=$(uuidgen)
        echo -e "${YELLOW}Generated UUID: $UUID${NC}"
        
        # 尝试写入UUID到Info.plist (需要sudo权限)
        echo -e "${YELLOW}Attempting to write UUID to Xcode...${NC}"
        sudo /usr/libexec/PlistBuddy -c "Add :DVTPlugInCompatibilityUUID string $UUID" "$XCODE_PLIST_PATH" 2>/dev/null
    else
        echo -e "${GREEN}Found Xcode UUID: $UUID${NC}"
    fi
    
    # 注意: 添加UUID到XVim2的兼容性列表
    echo -e "${BLUE}Adding UUID to XVim2 compatibility list...${NC}"
    if [ -f "XVim2/src/XVim/Info.plist" ]; then
        # 如果存在Info.plist，直接编辑
        /usr/libexec/PlistBuddy -c "Add :DVTPlugInCompatibilityUUIDs:0 string $UUID" "XVim2/src/XVim/Info.plist" 2>/dev/null
    else
        echo -e "${YELLOW}Info.plist not found in expected location, will rely on make process...${NC}"
        # 将UUID写入临时文件，让构建过程使用
        echo "$UUID" > uuid.txt
    fi
else
    echo -e "${RED}Xcode Info.plist not found at $XCODE_PLIST_PATH${NC}"
    exit 1
fi

# 构建XVim2
echo -e "${BLUE}Building XVim2...${NC}"
make

# 启用开发者模式
echo -e "${BLUE}Enabling Developer Mode (requires sudo)...${NC}"
sudo /usr/sbin/DevToolsSecurity -enable

# 创建自签名证书
echo -e "${BLUE}Creating self-signed certificate for Xcode signing...${NC}"

# 检查证书是否已存在
if security find-certificate -c "XcodeSigner" ~/Library/Keychains/login.keychain-db > /dev/null 2>&1; then
    echo -e "${GREEN}Certificate 'XcodeSigner' already exists.${NC}"
else
    # 使用security工具创建自签名证书
    echo -e "${BLUE}Creating new certificate...${NC}"
    security create-certificate -k ~/Library/Keychains/login.keychain-db -T /usr/bin/codesign -T /Applications/Xcode.app/Contents/MacOS/Xcode -s "XcodeSigner" -c R "XcodeSigner" -n "XcodeSigner" -b
    
    if [ $? -ne 0 ]; then
        echo -e "${RED}Failed to create certificate automatically.${NC}"
        echo -e "${YELLOW}Please create one manually using Keychain Access:${NC}"
        echo -e "1. Open Keychain Access"
        echo -e "2. Go to Keychain Access > Certificate Assistant > Create a Certificate"
        echo -e "3. Set name to 'XcodeSigner', Identity Type to 'Self Signed Root',"
        echo -e "   Certificate Type to 'Code Signing', and check 'Let me override defaults'"
        echo -e "4. Continue and create the certificate"
        echo -e "5. Then run: sudo codesign -f -s XcodeSigner /Applications/Xcode.app"
        exit 1
    fi
fi

# 签名Xcode
echo -e "${BLUE}Signing Xcode with the certificate (requires sudo)...${NC}"
sudo codesign -f -s XcodeSigner /Applications/Xcode.app

if [ $? -ne 0 ]; then
    echo -e "${RED}Xcode signing failed.${NC}"
    echo -e "${YELLOW}Try manually with: sudo codesign -f -s XcodeSigner /Applications/Xcode.app${NC}"
    exit 1
fi

echo -e "${GREEN}✓ Xcode signing completed successfully${NC}"

# 清理
echo -e "${BLUE}Cleaning up temporary files...${NC}"
cd ~
rm -rf ~/xvim_temp

echo -e "${GREEN}=== XVim2 installation completed! ===${NC}"
echo -e "${BLUE}Next steps:${NC}"
echo -e "1. ${YELLOW}Restart your computer${NC}"
echo -e "2. Start Xcode"
echo -e "3. If prompted about XVim2 plugin, select 'Load Bundle'"
echo -e "4. Test Vim commands (hjkl navigation, : for command mode)"
echo -e ""
echo -e "${BLUE}If XVim2 is still not working:${NC}"
echo -e "- Check if the plugin appears in Xcode > Settings > Extensions"
echo -e "- Try running: ${YELLOW}defaults delete com.apple.dt.Xcode DVTPlugInManagerNonApplePlugIns-Xcode-${XCODE_VERSION}${NC}"
echo -e "- Restart Xcode again" 