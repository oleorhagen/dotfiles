# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# remap return to return when pressed, and ctrl when pressed
# and remaps return to control when held, and ctrl when pressed
# save as ~/bin/ezrctls.sh (easier controls)

#!/bin/bash
# clear all mappings
setxkbmap -option ''
# assign return to control on press, return on release
xmodmap -e 'clear Lock'
xmodmap -e 'keycode 66 = Control_L'
xmodmap -e 'add Control = Control_L'
# make a fake escape key (so we can map it with xcape)
xmodmap -e 'keycode 999 = Escape'
xcape -e 'Control_L=Escape'

# assign return to control on press, return on release
xmodmap -e 'keycode 36 = 0x1234'
xmodmap -e 'add Control = 0x1234'
# make a fake return key (so we can map it with xcape)
xmodmap -e 'keycode 255 = Return'
xcape -e '0x1234=Return'
# depends xmodmap xcape
# https://github.com/alols/xcape
#
