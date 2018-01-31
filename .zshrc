# source .profile to enable the remapping of return and caps-lock
source ~/.profile # Consider adding this to an autorun-login shell-script
# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/$USER/.oh-my-zsh

export SCRIPT_DIR=/home/olepor/.i3blocks/i3blocks-contrib/

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="robbyrussell" # sammy

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=1

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git z docker docker-compose golang)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
#
# --------MENDER---------
# Source the oe-init-build-env with every new terminal-session
# source ~/poky/oe-init-build-env -- error, adds the build dir in home

# ------- Aliases -------
alias lsa="ls -a"
# Needs to have the BUILDDIR set
alias dockdev='BUILDDIR=/home/opus/ docker-compose -f docker-compose.yml -f docker-compose.storage.minio.yml -f docker-compose.demo.yml -f docker-compose.client.yml -f docker-compose.client-dev.yml run mender-client'
# add user to the integration test environment through the mender-useradm microservice
alias addusr='sudo docker-compose exec mender-useradm \
                         /usr/bin/useradm \
                         create-user \
                         --username=user@host.com \
                         --password=rootpass'
# Used for compiling gcc-cross-compiler for OPOS
#export PREFIX="$HOME/opt/cross"
#export TARGET=i686-elf
#export PATH="$PREFIX/bin:$PATH"
export GOROOT="/usr/lib/go-1.9"
export PATH="${GOROOT}/bin:$PATH"
export PATH="$HOME/opt/cross/bin:$PATH"

# Run neofetch on starting a shell
# neofetch
cd ~/poky && source oe-init-build-env && cd ~/

# Make funny on startup
fortune -a | cowsay
