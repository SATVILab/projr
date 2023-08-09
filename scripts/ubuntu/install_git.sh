#!/bin/bash
# source: https://learn.microsoft.com/en-us/windows/wsl/tutorials/wsl-git
# first install git on windows
sudo apt-get update 
sudo apt-get install -y git
git config --global init.defaultBranch main
# to reconcile pulls
git config pull.ff only
