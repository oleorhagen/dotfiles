#set -x
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
plugins=(git z docker docker-compose golang colored-man-pages)

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
alias open='xdg-open'
alias calculator='bc'
function calc () {
	case $# in
		0)
			bc
			;;
		*)
			bc -l <<< "$@"
			;;
	esac
}
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
    xrandr --output DP-2-2 --right-of DP-2-1 \
           --auto --output DP-2-1 --right-of eDP-1 \
           --rotate right --auto --output eDP-1 --auto
    # Move the workspaces to the correct monitors
    i3-msg '[workspace=7]' move workspace to output DP-2-1
    i3-msg '[workspace=8]' move workspace to output DP-2-2
    i3-msg '[workspace=9]' move workspace to output DP-2-2
    i3-msg '[workspace=10]' move workspace to output DP-2-2
}

# Aliases
if [[ -z "${WAYLAND_DISPLAY}" ]]; then
	alias pbcopy='xclip -selection clipboard'
	alias pbpaste='xclip -selection clipboard -o'
else
	alias pbcopy="wl-copy"
	alias pbpaste="wl-paste"
fi
# alias pip=pip3
alias ma='mender-artifact'

function dockercontainerlog() {
    docker logs $(docker ps -a | grep $1 | awk '{print $1}')
}

function dockergetcontainerid() {
    docker ps -a | grep $1 | cut -d' ' -f 1 | pbcopy && echo >&2 "hash copied to clipboard"
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
# xbindkeys --poll-rc

alias please="sudo"

alias ssh="TERM=xterm-256color ssh"

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

# Add cfengine binaries to the PATH
export PATH=/var/cfengine/bin:$PATH

# Add ~/code/scripts to the PATH
export PATH=${HOME}/code/scripts:$PATH

# Add the changelog-generator the the PATH
export PATH="${HOME}/mendersoftware/integration/extra/changelog-generator:${PATH}"

# Add the release-tool to the PATH
export PATH="${HOME}/mendersoftware/integration/extra:${PATH}"

# Automatically source Python virtualenv if present
function venv() {
    if [ -d .venv ]; then
        source .venv/bin/activate
    else
        echo >&2 "No .venv directory found in the current directory"
    fi
}

function released_debian_package_version_exists() {
	if [[ $# != 1 ]]; then
		echo >&2 "Usage: released_debian_package_version_exists version-tag"
	fi
	wget https://d1b0l86ne08fsf.cloudfront.net/$1/dist-packages/debian/armhf/mender-client_$1-1_armhf.deb
}

function acceptance-tests() {
	cd ~/mendersoftware/meta-mender/tests/acceptance
}
function integration-tests() {
	cd ~/mendersoftware/integration/tests
}
function yocto-build-folder() {
	cd ~/yocto/qemu/build
}

function makefile-verify() {
	make --warn-undefined-variables
}

function bitbake-list-image-packages() {
  if [ $# -ne 1 ]; then
    echo >&2 "No image/package given"
  fi
  bitbake -g "$1" && cat pn-depends.dot | grep -v -e '-native' | \
      grep -v digraph | grep -v -e '-image' | awk '{print $1}' | sort | uniq
}

function everyn () {
  if [[ $# -le 1 ]]; then
    echo >&2 "Usage: everyn seconds function"
    echo >&2 "NArgs: $#"
    return 1
  fi
  local TIME="$1"
  shift
  while :
  do
        "$@"
        sleep ${TIME}
  done
}

function appendworkdirtopath () {
  export PATH="${PWD}:$PATH"
}

if [[ ${RANDOM} -le 1000 ]]; then
	echo >&2 "Updating the firmware... (TODO - install this!)"
	#fwupdmgr update
fi

export GITLAB_TOKEN=$(pass show gitlab/accesstoken)


###############################################################################
#                 ZSH auto-completion stuff and configurations                #
###############################################################################

# folder of all of your autocomplete functions
fpath=($HOME/.zsh-completions $fpath)

# enable autocomplete function
autoload -U compinit
compinit
[ -d /opt/ros/melodic ] && source /opt/ros/melodic/setup.zsh

# ArduPilot paths
#export PATH=$PATH:$HOME/ardupilot/Tools/autotest
#export PATH=/usr/lib/ccache:$PATH
[ -d ~/catkin_ws ] && source ~/catkin_ws/devel/setup.zsh

# release-tool to PATH
export PATH=$PATH:$HOME/mendersoftware/integration/extra

#
# Source .mender.rc
#
source ~/dotfiles/scripts/.mender.rc

#
# Overrides
#

# Pip
function pip () {
  if [[ -z "${VIRTUAL_ENV}" ]]; then
    echo >&2 "Installing Python packages outside of a virtual environment is not allowed!"
    return 1
  fi
  pip "$@"
}

#
# Show the version of a mender-service in the given release version of Mender
# (Always a puzzle to figure out)
#
function menderversion () {
  if [[ $# -ne 2 ]]; then
    echo >&2 "Usage: menderversion <service> <release-version>"
  fi
  release_tool.py --version-of "$1" --in-integration-version "$2"
}

#
# Pretty cat <3
#
alias cat=bat

# Create a folder and move into it in one command
function mkcd() { mkdir -p "$@" && cd "$_"; }

# Emacsclient
#

#
# Load direnv
#
eval "$(direnv hook zsh)"
