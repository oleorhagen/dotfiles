# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022
export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin

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


# Rust to PATH
export PATH="$HOME/.cargo/bin:$PATH"

# RUST source code location (for auto-completion)
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
