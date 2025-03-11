#!/bin/bash

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # Reset color

echo -e "${BLUE}Installing XVim2 for Xcode...${NC}"

# Check if Xcode is installed
if ! [ -d "/Applications/Xcode.app" ]; then
    echo -e "${RED}Xcode is not installed in /Applications/Xcode.app${NC}"
    echo -e "${RED}Please install Xcode first.${NC}"
    exit 1
fi

# Create directory for XVim2
mkdir -p ~/xvim_temp
cd ~/xvim_temp

# Clone XVim2 repository
echo -e "${BLUE}Cloning XVim2 repository...${NC}"
git clone https://github.com/XVimProject/XVim2.git
cd XVim2

# Check Xcode version
XCODE_VERSION=$(xcodebuild -version | grep Xcode | cut -d ' ' -f 2)
echo -e "${BLUE}Detected Xcode version: ${XCODE_VERSION}${NC}"

# Build XVim2
echo -e "${BLUE}Building XVim2...${NC}"
make

# Guide for enabling Developer Mode
echo -e "${BLUE}Enabling Developer Mode (requires sudo)...${NC}"
sudo /usr/sbin/DevToolsSecurity -enable

# Instructions for code signing
echo -e "${GREEN}XVim2 build complete.${NC}"
echo -e "${BLUE}You need to enable code signing:${NC}"
echo -e "${GREEN}1. Open Xcode and agree to the license if prompted${NC}"
echo -e "${GREEN}2. Close Xcode${NC}"
echo -e "${GREEN}3. Run 'sudo codesign -f -s XcodeSigner /Applications/Xcode.app'${NC}"
echo -e "${GREEN}4. If you don't have XcodeSigner certificate, create it:${NC}"
echo -e "${GREEN}   - Open Keychain Access${NC}"
echo -e "${GREEN}   - Go to Keychain Access > Certificate Assistant > Create a Certificate${NC}"
echo -e "${GREEN}   - Set name to 'XcodeSigner', Identity Type to 'Self Signed Root',${NC}"
echo -e "${GREEN}     Certificate Type to 'Code Signing', and check 'Let me override defaults'${NC}"
echo -e "${GREEN}   - Continue and create the certificate${NC}"

# Copy .xvimrc
echo -e "${BLUE}Copying xvimrc file...${NC}"
cp ~/mydot/xvim/xvimrc ~/.xvimrc

# Clean up
echo -e "${BLUE}Cleaning up temporary files...${NC}"
cd ~
rm -rf ~/xvim_temp

echo -e "${GREEN}XVim2 installation script complete.${NC}"
echo -e "${GREEN}Please follow the code signing instructions above to complete the setup.${NC}"
echo -e "${GREEN}After that, restart Xcode and XVim should be active.${NC}" 