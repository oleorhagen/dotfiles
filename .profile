# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022
# export GOPATH=~/go
# export GOROOT="/usr/lib/go/"
# export PATH=$PATH:$GOPATH/bin

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
#export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
#export GPG_TTY=$(tty)
#export PATH=/opt/gcc-arm-none-eabi-7-2017-q4-major/bin:$PATH
#export PATH=/opt/gcc-arm-none-eabi-6-2017-q2-update/bin:$PATH
#export PATH=/home/olepor/misc/projects/quadcopter/ardupilot/Tools/autotest:$PATH
#export PATH=/usr/lib/ccache:$PATH

alias gpg=gpg2

#export GITLAB_TOKEN=`pass show -c gitlab/accesstoken`

export RIPGREP_CONFIG_PATH="${HOME}/.ripgreprc"

# gpg needs to know the tty
export GPG_TTY=$(tty)
