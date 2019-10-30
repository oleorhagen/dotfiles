#!/bin/bash

# Make sure the build environment is setup
cd ~/yocto/qemu-setup/
source poky/oe-init-build-env

# Save the current config directory to backup

# Use the standard local client setup from $HOME/resources

# Build the latest image
nice -n 15 bitbake core-image-full-cmdline
