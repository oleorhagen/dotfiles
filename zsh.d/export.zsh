#
# Exports
#

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# alias runqemu="~/"
# Used for compiling gcc-cross-compiler for OPOS
#export PREFIX="$HOME/opt/cross"
#export TARGET=i686-elf
#export PATH="$PREFIX/bin:$PATH"
#export GOROOT="/usr/lib/go-1.15/"
export PATH="$PATH:$(go env GOPATH)/bin:$GOROOT"
#export PATH=$GOPATH/bin:$GOROOT/bin:$PATH
# export PATH="/home/olepor/go:$PATH"
#export PATH="$PATH:/usr/local/go/bin"

# Run neofetch on starting a shell
# neofetch
# cd ~/poky && source oe-init-build-env && cd ~/

# Make funny on startup
#fortune -a | cowsay


# ccache should be in the $PATH before the compiler
export PATH="/usr/lib/ccache/bin:$PATH"

# Add dotfiles/scripts to PATH
export PATH=$HOME/.files/scripts:$PATH

# Add the misc/scripts folder to PATH
export PATH=$PATH:/home/olepor/misc/scripts

# Add ~/code/scripts to the PATH
export PATH=${HOME}/code/scripts:$PATH

# No python bytecode plz
export PYTHONDONTWRITEBYTECODE="true"

# Add the kube-ps1 prompt setup
#PROMPT='$(kube_ps1)'$PROMPT
#kubeoff # Don't add the prompt by default

# Set the kubeconfig to handle all config file .kube/config & .kube/config.d/*.yaml|yml
# TODO - kubectx does not work with multiple configuration files atm. So for now we need to keep them all in the top-level config file
# export KUBECONFIG="${HOME}/.kube/config$(for f in $(ls ${HOME}/.kube/config.d/); do echo -n ':'${HOME}/.kube/config.d/${f}; done)"
