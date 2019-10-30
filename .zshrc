# source .profile to enable the remapping of return and caps-lock
source ~/.profile # Consider adding this to an autorun-login shell-script
# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/home/$USER/.oh-my-zsh

export SCRIPT_DIR=/home/olepor/.i3blocks/i3blocks-contrib/

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="olepor" # sammy

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
alias bb='bitbake'
# alias runqemu="~/"
# Used for compiling gcc-cross-compiler for OPOS
#export PREFIX="$HOME/opt/cross"
#export TARGET=i686-elf
#export PATH="$PREFIX/bin:$PATH"
export GOROOT="/usr/lib/go-1.13/"
export PATH="$PATH:$(go env GOPATH)/bin:$GOROOT"
#export PATH=$GOPATH/bin:$GOROOT/bin:$PATH
# export PATH="/home/olepor/go:$PATH"
#export PATH="$PATH:/usr/local/go/bin"

# Run neofetch on starting a shell
# neofetch
# cd ~/poky && source oe-init-build-env && cd ~/

# Make funny on startup
fortune -a | cowsay


#export PATH=$PATH:/home/olepor/go/bin

#export PATH=/home/olepor/.go/bin:$PATH

# Simple function to output whatever is on the clip-board
# to the given file.
#function pasteto(destfile) {
#
#}
#
function inityocto() {
	cd ~/yocto/qemu-setup/
	source poky/oe-init-build-env
	cd -2
}

# Switches to and from the freestanding i3-config.
function i3switchconfig() {
    (
        set -vxe
        cd ~/.config/i3/
        tmpf=$(mktemp --suffix=i3conf)
        cp config $tmpf
        cp config.bak config
        cp $tmpf config.bak
        i3-msg restart
        # rm $tmpf
    )
}

function i3multiscreensetup() {
    set -xve
    xrandr --output DP-2-2 --left-of DP-2-1 \
           --auto --output DP-2-1 --left-of eDP-1 \
           --rotate right --auto --output eDP-1 --auto
}

# Aliases
alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'
alias pip=pip3
alias ma='mender-artifact'

function dockercontainerlog() {
    docker logs $(docker ps -a | grep $1 | awk '{print $1}')
}

function dockergetcontainerid() {
    docker ps -a | grep $1 | cut -d' ' -f 1 | pbcopy && echo "hash copied to clipboard"
}

# added by travis gem
[ -f /home/olepor/.travis/travis.sh ] && source /home/olepor/.travis/travis.sh

# Vagrant setup
export NTECH_ROOT="$HOME/northern.tech"

# Add dotfiles/scripts to PATH
export PATH=$HOME/dotfiles/scripts:$PATH

# Alias the sourcing of this file (.zshrc)
alias zshsrc='source ~/.zshrc'

# Make sure xbindkeys is sourced
xbindkeys --poll-rc

alias plz="sudo"

# Use explainshell.com to explain weird bash commands to me
function explain {
    # base url with first command already injected
    # $ explain tar
    #   => http://explainshel.com/explain/tar?args=
    url="http://explainshell.com/explain/$1?args="

    # removes $1 (tar) from arguments ($@)
    shift;

    # iterates over remaining args and adds builds the rest of the url
    for i in "$@"; do
        url=$url"$i""+"
    done

    # dump the explanation to the terminal. Strip off mumbojumbo
    w3m -dump "$url" | awk 'NR > 8 {print}'
}

function srcyocto {
    cd ~/yocto/qemu-setup
    source ./poky/oe-init-build-env build
}

# Add the misc/scripts folder to PATH
export PATH=$PATH:/home/olepor/misc/scripts
