#!/bin/bash

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # Reset color

echo -e "${BLUE}Complete XVim2 Setup for Xcode...${NC}"

# Check if Xcode is installed
if ! [ -d "/Applications/Xcode.app" ]; then
    echo -e "${RED}Xcode is not installed in /Applications/Xcode.app${NC}"
    echo -e "${RED}Please install Xcode first.${NC}"
    exit 1
fi

# First make sure .xvimrc is in place
cp ~/Desktop/mydot/xvim/xvimrc ~/.xvimrc
echo -e "${GREEN}Copied .xvimrc to home directory.${NC}"

# Create a script for certificate creation
cat > ~/create_certificate.sh << 'EOL'
#!/bin/bash

# Create a self-signed code signing certificate
echo "Creating code signing certificate for Xcode..."

# Check if certificate already exists
if security find-certificate -c "XcodeSigner" > /dev/null 2>&1; then
    echo "Certificate 'XcodeSigner' already exists."
else
    echo "Creating certificate 'XcodeSigner'..."
    # Create a certificate template
    cat > /tmp/certtmpl.plist << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Name</key>
    <string>XcodeSigner</string>
    <key>Subject</key>
    <array>
        <array>
            <array>
                <string>CN</string>
                <string>XcodeSigner</string>
            </array>
        </array>
    </array>
    <key>Serial</key>
    <integer>1</integer>
    <key>Issuer</key>
    <array>
        <array>
            <array>
                <string>CN</string>
                <string>XcodeSigner</string>
            </array>
        </array>
    </array>
</dict>
</plist>
EOF

    # Create certificate
    security create-certificate -k ~/Library/Keychains/login.keychain -p -n "XcodeSigner" -c R "XcodeSigner" -i /tmp/certtmpl.plist 2>/dev/null
    rm /tmp/certtmpl.plist
    
    # Check if certificate was created
    if security find-certificate -c "XcodeSigner" > /dev/null 2>&1; then
        echo "Certificate 'XcodeSigner' created successfully."
    else
        echo "Failed to create certificate automatically."
        echo "Please create one manually using Keychain Access."
        exit 1
    fi
fi

# Sign Xcode with the certificate
echo "Signing Xcode with the certificate..."
sudo codesign -f -s XcodeSigner /Applications/Xcode.app
echo "Xcode signing completed."

# Check if signing was successful
if [ $? -eq 0 ]; then
    echo "Xcode signing was successful."
else
    echo "Xcode signing failed. Please try manually:"
    echo "sudo codesign -f -s XcodeSigner /Applications/Xcode.app"
    exit 1
fi
EOL

chmod +x ~/create_certificate.sh

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

# Enable Developer Mode
echo -e "${BLUE}Enabling Developer Mode (requires sudo)...${NC}"
sudo /usr/sbin/DevToolsSecurity -enable

# Run the certificate creation and signing script
echo -e "${BLUE}Running certificate creation and Xcode signing...${NC}"
echo -e "${RED}This will require your password for sudo access.${NC}"
~/create_certificate.sh

# Clean up
echo -e "${BLUE}Cleaning up temporary files...${NC}"
cd ~
rm -rf ~/xvim_temp
rm ~/create_certificate.sh

echo -e "${GREEN}XVim2 installation complete.${NC}"
echo -e "${GREEN}Please restart Xcode to activate XVim2.${NC}"
echo -e "${RED}Important: If Xcode shows a dialog about the plugin, select 'Load Bundle'.${NC}"
echo -e "${BLUE}If XVim is still not working, try these steps:${NC}"
echo -e "1. Open Xcode and check for any dialogs or warnings"
echo -e "2. Check Xcode Preferences > Extensions for XVim2"
echo -e "3. If all else fails, try reinstalling with: make clean && make" 