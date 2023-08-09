#!/bin/bash
# source: https://code.visualstudio.com/docs/setup/linux#_debian-and-ubuntu-based-distributions
# date created: 2023 June 06
# last updated: 2023 June 06
sudo apt-get install -y wget gpg
wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
sudo install -D -o root -g root -m 644 packages.microsoft.gpg /etc/apt/keyrings/packages.microsoft.gpg
sudo sh -c 'echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" > /etc/apt/sources.list.d/vscode.list'
sudo apt-get update -y
sudo apt-get install -y apt-transport-https
sudo apt-get install -y code-insiders 
sudo apt-get install -y code 
rm -f packages.microsoft.gpg
