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


#export PATH=$PATH:/home/olepor/go/bin

#export PATH=/home/olepor/.go/bin:$PATH


# Vagrant setup
export NTECH_ROOT="$HOME/northern.tech"

# Add dotfiles/scripts to PATH
export PATH=$HOME/dotfiles/scripts:$PATH

# Add the misc/scripts folder to PATH
export PATH=$PATH:/home/olepor/misc/scripts

# Add cfengine binaries to the PATH
export PATH=/var/cfengine/bin:$PATH

# Add ~/code/scripts to the PATH
export PATH=${HOME}/code/scripts:$PATH

# Add the changelog-generator the the PATH
export PATH="${HOME}/mendersoftware/integration/extra/changelog-generator:${PATH}"

# Add the release-tool to the PATH
export PATH="${HOME}/mendersoftware/integration/extra:${PATH}"

export GITLAB_TOKEN=$(pass show private/gitlab/access-token)

# release-tool to PATH
export PATH=$PATH:$HOME/mendersoftware/integration/extra

# No python bytecode plz
export PYTHONDONTWRITEBYTECODE="true"

# Export a Mender auth token as a JWT
export JWT="$(pass show private/hosted-mender/access-token)"

