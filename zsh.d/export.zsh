#
# Exports
#

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# git
## Which branch to use as the review base when running custom git commands
export REVIEW_BASE="master"

# alias runqemu="~/"
# Used for compiling gcc-cross-compiler for OPOS
#export PREFIX="$HOME/opt/cross"
#export TARGET=i686-elf
#export PATH="$PREFIX/bin:$PATH"
#export GOROOT="/usr/lib/go-1.15/"
# export PATH="$PATH:$(go env GOPATH)/bin:$GOROOT"
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

# Add cisco scripts to PATH
export PATH=$PATH:/home/$(whoami)/.files/scripts/cisco

# Add the misc/scripts folder to PATH
export PATH=$PATH:/home/$(whoami)/misc/scripts

# Add ~/code/scripts to the PATH
export PATH=${HOME}/code/scripts:$PATH

# Add go bin to the PATH
export PATH=$PATH:$(go env GOPATH)/bin

# No python bytecode plz
export PYTHONDONTWRITEBYTECODE="true"

# Add the kube-ps1 prompt setup
#PROMPT='$(kube_ps1)'$PROMPT
#kubeoff # Don't add the prompt by default

# Set the kubeconfig to handle all config file .kube/config & .kube/config.d/*.yaml|yml
# TODO - kubectx does not work with multiple configuration files atm. So for now we need to keep them all in the top-level config file
# export KUBECONFIG="${HOME}/.kube/config$(for f in $(ls ${HOME}/.kube/config.d/); do echo -n ':'${HOME}/.kube/config.d/${f}; done)"

## AUTOENV use .in and .out files
export AUTOENV_ENV_FILENAME=.in
export AUTOENV_ENV_LEAVE_FILENAME=.out

# MIMIRO

# Set aws-vault to use pass as the default backend
export AWS_VAULT_BACKEND=pass

#
# Claude code export variables
#
export CLAUDE_CODE_USE_BEDROCK=1  # configure Claude to use Bedrock
export CLAUDE_CODE_SKIP_BEDROCK_AUTH=1  # don't use AWS IAM auth to access Bedrock
export ANTHROPIC_BEDROCK_BASE_URL=https://llm-proxy.us-east-2.ops.infra.intelligence.webex.com/bedrock/v1
# CLAUDE_CODE_API_KEY_HELPER_TTL_MS=300000  # 5 minutes
