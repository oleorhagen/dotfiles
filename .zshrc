# Debug
#set -x
#

#
# Show Logo on login shell
#
if cat /etc/os-release | grep -i nixos &>/dev/null; then
    echo "Welcome to NixOS! (Time to get a pretty logo)"
else
    # Assume Arch Linux
    archey3 --color=green
fi

# source .profile to enable the remapping of return and caps-lock
source ~/.profile # Consider adding this to an autorun-login shell-script
# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
# export ZSH=/home/$USER/.oh-my-zsh

export SCRIPT_DIR=/home/olepor/.i3blocks/i3blocks-contrib/

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="olepor" # sammy
ZSH_THEME="sammy" # sammy

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=10

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

# source $ZSH/oh-my-zsh.sh

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    z
    docker
    docker-compose
    golang
    colored-man-pages
    per-directory-history
    autoenv # .in and .out files to automatically setup an env when entering a directory
    aws
)

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

if [[ ${RANDOM} -le 1000 ]]; then
	echo >&2 "Updating the firmware..."
	fwupdmgr update
fi


###############################################################################
#                 ZSH auto-completion stuff and configurations                #
###############################################################################

# folder of all of your autocomplete functions
fpath=($HOME/.zsh-completions $fpath)


#
# Source .mender.rc
#
#source ~/dotfiles/scripts/.mender.rc

# Emacsclient
#

#
# Load direnv
#
eval "$(direnv hook zsh)"

#
# Mender client releases
#
#if [ ! -e /tmp/client-releases-printed ]; then
#    ~/dotfiles/scripts/client-releases
#    touch /tmp/client-releases-printed
#fi

# Source all the individual configuration files
for file in ~/.files/zsh.d/*; do
    source $file
done

#
# K8s config
#

#
# Source kubectl auto completion
#
source <(kubectl completion zsh)

# Use starship as the prompt provider
eval "$(starship init zsh)"

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/bin/mcli mcli
